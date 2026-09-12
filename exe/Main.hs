{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: make dropout ratio configurable
-- TODO: allow for different droput policies
-- TODO: add interface for disabling search
-- TODO: add interface for disabling stdio logging of hlo-opt
-- TODO: pull logic out into separate files. This one is getting crowded
-- TODO: make ETA interval configurable
-- TODO: add nicer ETA display
-- TODO: implement bridge and neck policy correctly
-- TODO: make "fuse all" configurable

module Main where

import qualified Graph as G
import Unique
import Types
import RecPart

import Control.Concurrent          (Chan(), writeChan, readChan, newChan, threadDelay)
import Control.Concurrent.Async    (wait, withAsync)
import Control.Concurrent.STM      (readTVarIO, writeTVar, TVar, readTVar, atomically, newTVarIO)
import Data.Bifunctor              (first)
import Data.Time                   (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Conc                    (numCapabilities)
import System.Directory            (doesFileExist, removeFile, createDirectoryIfMissing, getCurrentDirectory, makeAbsolute)
import System.Environment          (lookupEnv, getArgs, getEnvironment)
import System.Exit                 (ExitCode(..))
import System.IO                   (Handle, withFile, IOMode(WriteMode))
import System.Process              (CreateProcess(..), StdStream(UseHandle), createProcess_, waitForProcess, proc)
import System.Random               (StdGen, mkStdGen)

import qualified Data.Map as M


splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn _ [] = ([], [])
splitOn k (x:xs) = if x == k
    then ([], xs)
    else (x:ls, rs)
    where
        (ls, rs) = splitOn k xs


dropout :: Ord v => Double -> G.Graph v -> G.Graph v
dropout ratio g = case G.getSubgraphs dropped of
    [x] -> x
    []  -> error "Dropout failed: No graph left"
    _   -> error "Dropout failed: We split the graph"
    where
        edges = G.getEdgesTopological g
        edge_count = length edges
        to_remove = round $ fromIntegral edge_count * ratio
        elems = take to_remove edges
        dropped = foldl (flip ($)) g $ uncurry G.removeEdge <$> elems


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

    let num_edges = length $ G.getEdges graph
    putStrLn $ "Read computation '" ++ compname ++ "' with " ++ show num_edges ++ " edges"

    let graph' = dropout 0.0 graph
    let num_edges' = length $ G.getEdges graph'
    putStrLn $ "Dropped out to " ++ show num_edges' ++ " edges"

    eval_counter <- newTVarIO 0

    let !total_eval_count =
            let compute = recPart thread_budget rng_gen merge eval_eval_c graph'
                (_, res) = runCounterM compute
            in res

    if calc_eval_count
        then return $ Left total_eval_count
        else do
            log_channel <- newChan

            let compute = recPart thread_budget rng_gen merge (eval eval_counter base_env log_channel compname) graph'

            start_time <- getCurrentTime
            withAsync (log_thread log_channel) $ \logger ->
                withAsync (update_thread eval_counter total_eval_count start_time) $ \_ -> do
                    res <- compute
                    writeChan log_channel LogEnd
                    wait logger
                    return $ Right res
    where
        update_thread :: TVar Int -> Int -> UTCTime -> IO ()
        update_thread counter total_evals start_time = go
            where
                go :: IO ()
                go = do
                    threadDelay 1000000
                    time_now <- getCurrentTime
                    counter_now <- readTVarIO counter
                    let duration = time_now `diffUTCTime` start_time
                    putStrLn
                        $ "Running for "
                        ++ show duration
                        ++ ": (" ++ show counter_now ++ "/" ++ show total_evals ++ ")"
                    go


        rng_gen :: StdGen
        rng_gen = mkStdGen 0xC0A71

        graph_dump_file :: FilePath
        graph_dump_file = workdir ++ "/graph"

        eval :: TVar Int -> [(String, String)] -> Chan LogMsg -> String -> Unique -> FuseNoFuses Reg -> IO Quality
        eval eval_counter base_env log_channel cname unique fnf = do

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

                    --writeChan log_channel $ LogEval fnf raw_stats quality
                    atomically $ do
                        old <- readTVar eval_counter
                        writeTVar eval_counter (old + 1)

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



