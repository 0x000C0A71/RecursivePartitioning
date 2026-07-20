{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Graph as G
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Bifunctor
import Control.Parallel
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.State

import System.Process
import Data.IORef
import System.Environment (setEnv, unsetEnv, lookupEnv, getArgs)
import System.Directory

--liftA2 :: Applicative a => (b -> c -> d) -> a b -> a c -> a d
--liftA2 fn l r = fn <$> l <*> r

data Reg
    = OrigReg String
    | RenameReg String
    deriving (Show, Eq, Ord)

type Counter = IORef Int

type Quality = Float

counterInc :: Counter -> IO Int
counterInc counter = do
    old <- readIORef counter
    writeIORef counter $ old + 1
    return old

newCounter :: IO Counter
newCounter = newIORef 0

-- recPart :: forall v m q . (Ord v, Ord q, Monad m) => (v -> v -> m v) -> (FuseNoFuses v -> m q) -> G.Graph v -> m (FuseNoFuses v)

withEnv :: String -> String -> IO a -> IO a
withEnv en ev act = do
    setEnv en ev
    v <- act
    unsetEnv en
    return v


splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn k [] = ([], [])
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
        _ -> return $ error "Expected path to hlo module as cmd line arg"

    workdir <- getCurrentDirectory >>= makeAbsolute


    fnf <- runOn hlo_opt hlo_opt_args workdir hlo_path
    print fnf

runOn :: FilePath -> [String] -> FilePath -> FilePath -> IO (FuseNoFuses Reg)
runOn hlo_opt hlo_opt_args workdir hlo_path = do
    writeFile fusion_log_file ""

    prefix_counter <- newCounter
    register_counter <- newCounter

    withEnv "XLA_RPOF_FORWARD_FILE" graph_dump_file $
        call_opt

    [(compname, graph)] <- M.toList . readGraphs <$> readFile graph_dump_file

    recPart (merge register_counter) (eval compname prefix_counter) graph
    where
        graph_dump_file :: FilePath
        graph_dump_file = workdir ++ "/graph"

        fusion_log_file = workdir ++ "/fusion-log"

        eval :: String -> Counter -> FuseNoFuses Reg -> IO Quality
        eval cname pc fnf = do
            appendFile fusion_log_file $ "Attempting " ++ show fnf ++ "... "

            cv <- counterInc pc
            let instr_file = workdir ++ "/fnf" ++ show cv
            let out_file = workdir ++ "/force_out" ++ show cv
            writeFile instr_file $ encode cname $ first reverse  fnf

            withEnv "XLA_RPOF_FORCE_FILE" instr_file $ withEnv "XLA_RPOF_QUALITY_FILE" out_file $
                call_opt

            quality <- read . head . lines <$> readFile out_file

            removeFile instr_file
            removeFile out_file

            appendFile fusion_log_file $ show quality ++ "\n"

            return quality

        merge :: Counter -> Reg -> Reg -> IO Reg
        merge rc _ _ = RenameReg . ("tmp" ++) . show <$> counterInc rc

        call_opt :: IO ()
        call_opt = callProcess hlo_opt $ hlo_opt_args ++ [hlo_path]



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


type InstrName = String


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




class Monad m => MPar m where
    mpar :: m a -> m b -> m (a, b)
    mpar = liftA2 (,)

    parseq :: [m a] -> m [a]
    parseq [] = return []
    parseq [x] = (:[]) <$> x
    parseq (x:xs) = uncurry (:) <$> mpar x (parseq xs)



data Future a = Future (MVar a) ThreadId

futureAwait :: Future a -> IO a
futureAwait (Future mv _) = readMVar mv

futureCancel :: Future a -> IO ()
futureCancel (Future _ ti) = killThread ti

async :: IO a -> IO (Future a)
async act = do
    mv <- newEmptyMVar
    ti <- forkFinally act $ \case
        Right v -> putMVar mv v
        Left e  -> putMVar mv $ error "async exception"
    return $ Future mv ti

awaitAll :: [Future a] -> IO [a]
awaitAll = mapM futureAwait

instance MPar IO where
    mpar l r = do
        fl <- async l
        fr <- async r
        vl <- futureAwait fl
        vr <- futureAwait fr
        return (vl, vr)

    parseq v = do
        fs <- mapM async v
        awaitAll fs



data HloModule

findOptimal :: HloModule -> IO (S.Set (v, v), S.Set (v, v))
findOptimal = undefined
    where
        --eval


type Fusion v = (v, v, v)
type NoFusion v = (v, v)
type FuseNoFuses v = ([Fusion v], S.Set (NoFusion v))

recPart :: forall v m q . (Ord v, Ord q, Monad m) => (v -> v -> m v) -> (FuseNoFuses v -> m q) -> G.Graph v -> m (FuseNoFuses v)
recPart merge eval = fmap snd . go ([], S.empty)
    where
        go :: FuseNoFuses v -> G.Graph v -> m (q, FuseNoFuses v)
        go !f !g = case pick_edge g of
            Nothing -> (,f) <$> eval f
            Just (from, to) -> do
                merged <- merge from to

                let with_merged = first ((from, to, merged):) f
                let with_split = second (S.insert (from, to)) f

                (merged_quality, merged_sets) <- go with_merged $ G.mergeEdge from to merged g
                (split_quality , split_sets ) <- case G.getSubgraphs $ G.removeEdge from to g of
                    [x] -> go with_split x
                    xs -> do
                        rec_results <- go with_split `mapM` xs
                        let sets = bimap concat S.unions $ unzip $ snd <$> rec_results
                        quality <- eval sets
                        return (quality, sets)
                return $ if split_quality > merged_quality
                    then (split_quality, split_sets)
                    else (merged_quality, merged_sets)

        -- TODO: be more clever
        pick_edge :: G.Graph v -> Maybe (v, v)
        pick_edge g = case G.getEdges g of
            []    -> Nothing
            (e:_) -> Just e

