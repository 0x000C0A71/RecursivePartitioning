{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Graph as G
import Unique

import Control.Concurrent          (Chan(), writeChan, readChan, newChan)
import Control.Concurrent.Async    (wait, withAsync, mapConcurrently)
import Control.Monad.Reader        (ReaderT(), ask, runReaderT)
import Control.Monad.Trans         (lift)
import Control.Monad.Writer        (Writer(), writer, runWriter, execWriter)
import Control.Parallel.Strategies (using, parTuple2, evalTuple2, rseq, rdeepseq)
import Data.Bifunctor              (first, second, bimap)
import Data.Foldable               (minimumBy)
import Data.Monoid                 (Sum(Sum))
import GHC.Conc                    (numCapabilities)
import System.Directory            (doesFileExist, removeFile, createDirectoryIfMissing, getCurrentDirectory, makeAbsolute)
import System.Environment          (lookupEnv, getArgs, getEnvironment)
import System.Exit                 (ExitCode(..))
import System.IO                   (Handle, withFile, IOMode(WriteMode))
import System.Process              (CreateProcess(..), StdStream(UseHandle), createProcess_, waitForProcess, proc)
import System.Random               (RandomGen, uniformR, StdGen, mkStdGen)

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Control.Parallel.Strategies as PS (parList)
import qualified System.Random as R (split)

data Reg
    = OrigReg String
    | RenameReg String
    deriving (Show, Eq, Ord)

type Quality = Float


splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn _ [] = ([], [])
splitOn k (x:xs) = if x == k
    then ([], xs)
    else (x:ls, rs)
    where
        (ls, rs) = splitOn k xs


main :: IO ()
main = do
    hlo_opt <- flip fmap (lookupEnv "HLO_OPT_PATH") $ \case
        Just p -> p
        Nothing -> "hlo_opt"

    (my_args, hlo_opt_args) <- splitOn "--" <$> getArgs

    hlo_path <- case my_args of
        [path] -> makeAbsolute path
        _ -> return $ error "Expected path to hlo module as singular cmd line arg"

    workdir <- getCurrentDirectory >>= makeAbsolute

    let max_budget = fromIntegral $ numCapabilities * 4

    fnf <- runOn False max_budget hlo_opt hlo_opt_args workdir hlo_path
    print fnf


data LogMsg
    = LogEval (FuseNoFuses Reg) [Int] Quality
    | LogEnd
    | LogNoOutput String
    deriving (Show)


runOn :: Bool -> Budget -> FilePath -> [String] -> FilePath -> FilePath -> IO (Either (Int) (FuseNoFuses Reg))
runOn calc_eval_count thread_budget hlo_opt hlo_opt_args workdir hlo_path = do
    createDirectoryIfMissing True opt_logs

    base_env <- getEnvironment

    call_opt "forward-pass" $ ("XLA_RPOF_FORWARD_FILE", graph_dump_file) : base_env

    [(compname, graph)] <- M.toList . readGraphs <$> readFile graph_dump_file

    if calc_eval_count
        then let
            compute = recPart thread_budget rng_gen merge eval_eval_c graph
            Sum res = execWriter compute
            in return $ Left res
        else do
            log_channel <- newChan

            let compute = recPart thread_budget rng_gen merge (eval base_env log_channel compname) graph

            withAsync (log_thread log_channel) $ \logger -> do
                res <- compute
                writeChan log_channel LogEnd
                wait logger
                return $ Right res
    where
        rng_gen :: StdGen
        rng_gen = mkStdGen 0xC0A71

        graph_dump_file :: FilePath
        graph_dump_file = workdir ++ "/graph"

        eval :: [(String, String)] -> Chan LogMsg -> String -> Unique -> FuseNoFuses Reg -> IO Quality
        eval base_env log_channel cname unique fnf = do

            let instr_file = workdir ++ "/fnf" ++ show unique
            let out_file = workdir ++ "/force_out" ++ show unique
            writeFile instr_file $ encode cname $ first reverse  fnf

            call_opt ("eval-" ++ show unique)
                $ ("XLA_RPOF_FORCE_FILE"  , instr_file)
                : ("XLA_RPOF_QUALITY_FILE", out_file)
                : ("XLA_RPOF_COMPUTATION" , cname)
                : base_env


            doesFileExist out_file >>= \case
                True -> do
                    raw_stats :: [Int] <- fmap read . lines <$> readFile out_file
                    let [leaf_instrs, num_kernels, num_launches, bytes_read, bytes_written, flops, exec_nanos] :: [Float] = fromIntegral <$> raw_stats
                    let quality = 1.0/exec_nanos

                    removeFile instr_file
                    removeFile out_file

                    writeChan log_channel $ LogEval fnf raw_stats quality

                    return quality
                False -> do
                    writeChan log_channel $ LogNoOutput $ show unique
                    return 0




        merge :: Monad m => Unique -> Reg -> Reg -> m (Reg, Unique)
        merge u _ _ = return (RenameReg $ "tmp" ++ show u, u')
            where
                u' = next u

        eval_eval_c :: Unique -> FuseNoFuses v -> CounterM Int
        eval_eval_c _ _ = inc >> return 0

        opt_logs :: FilePath
        opt_logs = workdir ++ "/opt-logs"

        call_opt :: String -> [(String, String)] -> IO ()
        call_opt suffix opt_env =
            withFile opt_out WriteMode $ \opt_out_hdl ->
            withFile opt_err WriteMode $ \opt_err_hdl -> do
                let cp = create_process opt_out_hdl opt_err_hdl
                (_, _, _, process_handle) <- createProcess_ "call_opt" cp
                waitForProcess process_handle >>= \case
                    ExitSuccess -> return ()
                    ExitFailure e -> do
                        print cp
                        error $ "opt failed with exit code " ++ show e ++ ". Logs at " ++ opt_out ++ " & " ++ opt_err
            where
                create_process :: Handle -> Handle -> CreateProcess
                create_process opt_out_hdl opt_err_hdl = (proc hlo_opt $ hlo_opt_args ++ [hlo_path])
                    { std_out = UseHandle opt_out_hdl
                    , std_err = UseHandle opt_err_hdl
                    , env     = Just opt_env
                    }

                opt_out = opt_logs ++ "/opt-stdout-" ++ suffix
                opt_err = opt_logs ++ "/opt-stderr-" ++ suffix

        log_thread :: Chan LogMsg -> IO ()
        log_thread c = readChan c >>= \case
            LogEnd -> return ()
            other -> (>> log_thread c) $ case other of
                LogEval fnf metrics qual -> putStrLn $ show fnf ++ " " ++ show metrics ++ " yielded: " ++ show qual
                LogNoOutput suff -> putStrLn $ "!!! " ++ suff ++ " produced no output! Inspect!"




--(Reg, Reg, Reg)
encode :: String -> FuseNoFuses Reg -> String
encode cname (xs, _) = unlines $ do_one <$> xs
    where
        do_one :: Fusion Reg -> String
        do_one (_, _, OrigReg _) = error "error"
        do_one (from, to, RenameReg new) = unlines
            [ cname
            , fs
            , show fi
            , ts
            , show ti
            , new
            ]
            where
                (fs, fi :: Int) = case from of
                    OrigReg   s -> (s, 0)
                    RenameReg s -> (s, 1)
                (ts, ti :: Int) = case to of
                    OrigReg   s -> (s, 0)
                    RenameReg s -> (s, 1)




type ParserState = (String, Reg, M.Map String (G.Graph Reg))


readGraphs :: String -> M.Map String (G.Graph Reg)
readGraphs = (\(_,_,v) -> v) . flip (foldl (flip (.)) id . fmap one_line . lines) (undefined, undefined, M.empty)
    where
        one_line :: String -> ParserState -> ParserState
        one_line [] k = k
        one_line ('!':rest) (_   , _ , graphs) = (rest, undefined, M.insert rest G.empty graphs)
        one_line ('%':rest) (comp, _ , graphs) = (comp, OrigReg $ head $ words rest, graphs)
        one_line ('$':rest) (comp, to, graphs) = (comp, to, M.adjust (G.addEdge from to) comp graphs)
            where
                from = OrigReg $ head $ words rest
        one_line (c:_) _ = error $ "Malformed graph dump: Line starting with " ++ show c



type CounterM = Writer (Sum Int)

instance MonadPar CounterM where
    par2 (act1, act2) = writer ((a, b), w1 <> w2)
        where
            ((a, w1), (b, w2)) = (runWriter act1, runWriter act2) `using` parTuple2 seq_tup seq_tup
            seq_tup = evalTuple2 rseq rdeepseq

    parList acts = writer (els, mconcat counts)
        where
            parred = fmap runWriter acts `using` PS.parList seq_tup
            (els, counts) = unzip parred
            seq_tup = evalTuple2 rseq rdeepseq

add :: Int -> CounterM ()
add n = writer ((), Sum n)


inc :: CounterM ()
inc = add 1




class Monad m => MonadPar m where
    par2 :: (m a, m b) -> m (a, b)
    parList :: [m a] -> m [a]

    par3 :: (m a, m b, m c) -> m (a, b, c)
    par3 (a, b, c) = flip fmap (par2 (par2 (a, b), c)) $ \((a', b'), c') -> (a', b', c')

    par4 :: (m a, m b, m c, m d) -> m (a, b, c, d)
    par4 (a, b, c, d) = flip fmap (par2 (par2 (a, b), par2 (c, d))) $ \((a', b'), (c', d')) -> (a', b', c', d')

    par5 :: (m a, m b, m c, m d, m e) -> m (a, b, c, d, e)
    par5 (a, b, c, d, e) = flip fmap (par2 (par4 (a, b, c, d), e)) $ \((a', b', c', d'), e') -> (a', b', c', d', e')

instance MonadPar IO where
    par2 (act1, act2) = withAsync act2 $ \thread -> do
        res1 <- act1
        res2 <- wait thread
        return (res1, res2)

    parList = mapConcurrently id


type Budget = Double






type Fusion v = (v, v, v)
type NoFusion v = (v, v)
type FuseNoFuses v = ([Fusion v], S.Set (NoFusion v))

-- | Perform recursive partitioning on a graph
--
-- Searches for the optimal set of edges to fuse such that a
-- quality metric returned by the passed eval function is maximized
recPart :: forall v m q . (Ord v, Ord q, Monad m, MonadPar m)
    => Budget -- ^ Parallel bifurcation budget
    -> StdGen -- ^ Random number generator.
    -> (Unique -> v -> v -> m (v, Unique))
    -- ^ Merge function.
    -- When merging an edge of the graph, this function
    -- is used to generate the newly created "merged"
    -- vertex. A `Unique` ID source is provided which
    -- must be returned, both if stepped using `next`
    -- or if left unchanged.
    -> (Unique -> FuseNoFuses v -> m q)
    -- ^ Eval function
    -- When evaluating a leaf, this function is used to get
    -- a "quality" metric. It is provided with a `Unique` ID
    -- source, but it is not allowed to step it (as indicated
    -- by it not returning a `Unique`)
    -> G.Graph v -- ^ Graph to run the search on
    -> m (FuseNoFuses v)
--{-# SPECIALIZE recPart @Reg @IO @Quality #-}
--{-# SPECIALIZE recPart @Reg @CounterM @Int #-}
{-# SPECIALIZE recPart ::  Budget -> StdGen -> (Unique -> Reg -> Reg -> IO (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> IO Quality) -> G.Graph Reg -> IO (FuseNoFuses Reg) #-}
{-# SPECIALIZE recPart ::  Budget -> StdGen -> (Unique -> Reg -> Reg -> CounterM (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> CounterM Int) -> G.Graph Reg -> CounterM (FuseNoFuses Reg) #-}
recPart bud gen merge eval = fmap snd . go bud gen ([], S.empty) newUnique newUnique
    where
        go :: Budget -> StdGen -> FuseNoFuses v -> Unique -> Unique -> G.Graph v -> m (q, FuseNoFuses v)
        go !budget !rng !f !merge_u !eval_u !g = case edge_policy rng g of
            Nothing -> (,f) <$> eval eval_u f
            Just (from, to, rng') -> do
                (merged, merge_u') <- merge merge_u from to

                let with_merged = first ((from, to, merged):) f
                let with_split = second (S.insert (from, to)) f

                let eval_u' = next eval_u
                let (eval_u1, eval_u2) = split2 eval_u'

                let (rng1, rng2) = R.split rng'
                let merged_act = go (budget/2) rng1 with_merged merge_u' eval_u1 $ G.mergeEdge from to merged g
                ((merged_quality, merged_sets), (split_quality , split_sets)) <- case G.getSubgraphs $ G.removeEdge from to g of
                    []  -> do
                        mres <- merged_act
                        quality <- eval eval_u f
                        return (mres, (quality, f))
                    [x] ->
                        let unmerged_act = go (budget/2) rng2 with_split merge_u' eval_u2 x
                        in if budget > 1
                            then par2 (merged_act, unmerged_act)
                            else (,) <$> merged_act <*> unmerged_act
                    [x1, x2] -> do
                        let (eval_us1,  eval_us2 ) = split2 eval_u2
                        let (merge_us1, merge_us2) = split2 merge_u'
                        let go' = go (budget/4) rng2 with_split
                        let act1 = go' merge_us1 eval_us1 x1
                        let act2 = go' merge_us2 eval_us2 x2
                        (mres, (_, (fuse1, nfuse1)), (_, (fuse2, nfuse2))) <- case (budget > 1, budget > 2) of
                            (True , True ) -> par3 (merged_act, act1, act2)
                            (True , False) -> (\(a, (b, c)) -> (a, b, c)) <$> par2 (merged_act, (,) <$> act1 <*> act2)
                            (False, False) -> do
                                m  <- merged_act
                                s1 <- act1
                                s2 <- act2
                                return (m, s1, s2)
                            (False, True ) -> error "Not possible by transitivity of (>)"
                        let sets = (fuse1 ++ fuse2, nfuse1 `S.union` nfuse2)
                        quality <- eval eval_u sets
                        return (mres, (quality, sets))
                    _ -> error "Got more than 2 subgraphs after one deletion"
                return $ if split_quality > merged_quality
                    then (split_quality, split_sets)
                    else (merged_quality, merged_sets)

        edge_policy = edge_policy_mass

        -- | The whole algorithm "chops" one edge away each iteration,
        -- hoping to "chop" the graph in half. This function "aims" the
        -- "axe". The main goal is to "chop" the graph in two as fast as
        -- possible because that's where the speedup comes from.
        -- `pick_edges` uses the "mass" heuristic, trying to find the edge
        -- which has the lowest "mass"-difference (upstream "mass" of
        -- producer minus downstream "mass" of consumer). While this "mass"
        -- metric is relatively bad for getting equally-sized halves, it is
        -- rather quick to compute, and the idea is, to instead of finding
        -- the perfect place to "chop", we just "roughly aim for the center",
        -- and will probably get two parts rather quickly.
        edge_policy_mass :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_mass rng g = case G.getEdges g of
            []    -> Nothing
            edges ->
                let (x, y) = minimumWith evalEdge edges
                in Just (x, y, rng)
            where
                (intos, outs) = G.getMassMaps g

                evalEdge (f, t) = abs $ (intos M.! f) - (outs M.! t)

                minimumWith :: (Ord b, Foldable t) => (a -> b) -> t a -> a
                minimumWith fn = minimumBy $ \l r -> compare (fn l) (fn r)

        edge_policy_random :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_random rng g = case G.getEdges g of
            []    -> Nothing
            edges ->
                let
                    edge_count = length edges
                    (idx, rng') = uniformR (0, edge_count-1) rng
                    (x, y) = edges !! idx
                in Just (x, y, rng')

