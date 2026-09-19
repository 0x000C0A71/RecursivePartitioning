{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: allow for different droput policies
-- TODO: implement bridge and neck policy correctly
-- TODO: replace ai-generated code with human-generated code

module Main where

import qualified Graph as G
import Unique
import Types
import RecPart
import Parse

import Control.Concurrent          (Chan(), writeChan, readChan, newChan, threadDelay)
import Control.Concurrent.Async    (wait, withAsync)
import Control.Concurrent.STM      (readTVarIO, writeTVar, TVar, readTVar, atomically, newTVarIO)
import Data.Bifunctor              (first)
import Data.Time                   (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import System.Directory            (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Environment          (lookupEnv, getArgs, getEnvironment)
import System.Exit                 (ExitCode(..))
import System.IO                   (withFile, IOMode(WriteMode))
import System.Process              (CreateProcess(..), StdStream(UseHandle, NoStream), createProcess_, waitForProcess, proc)
import System.Random               (StdGen, mkStdGen)
import Data.Foldable               (find, maximumBy)
import Data.Ord                    (comparing)
import Control.Exception           (try, SomeException)


splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn _ [] = ([], [])
splitOn k (x:xs) = if x == k
    then ([], xs)
    else (x:ls, rs)
    where
        (ls, rs) = splitOn k xs

{- START AI-GENERATED CODE -}
-- | Reduce the graph to roughly @(1 - ratio)@ of its edges, choosing which
-- edges to drop according to the given policy.
dropout :: Ord v => DropoutPolicy -> Double -> G.Graph v -> G.Graph v
dropout policy ratio g = largestPiece
    $ foldl (flip ($)) g
    $ uncurry G.removeEdge <$> elems
    where
        edges      = G.getEdgesTopological g
        edge_count = length edges
        to_remove  = round $ fromIntegral edge_count * ratio
        elems = case policy of
            -- Drop the topologically first (input-facing) edges, keeping the
            -- output end.
            DropoutBeginning -> take to_remove edges
            -- Keep a contiguous window in the middle. The sources and sinks are
            -- the least representative part of a computation, so drop equally
            -- from both ends rather than everything from one.
            DropoutCenter ->
                let from_head = to_remove `div` 2
                    from_tail = to_remove - from_head
                in take from_head edges ++ drop (edge_count - from_tail) edges

        -- | Keep the largest connected piece of a possibly-disconnected graph.
        largestPiece :: Ord v => G.Graph v -> G.Graph v
        largestPiece g = case G.getSubgraphs g of
            [] -> error "Dropout failed: No graph left"
            xs -> maximumBy (comparing (length . G.getEdges)) xs
{- END AI-GENERATED CODE -}

main :: IO ()
main = do
    hlo_opt <- flip fmap (lookupEnv "HLO_OPT_PATH") $ \case
        Just p -> p
        Nothing -> "hlo_opt"

    (my_args, hlo_opt_args) <- splitOn "--" <$> getArgs

    config <- makeConfigAbsolute $ parseArgs my_args

    runOn config hlo_opt hlo_opt_args


data LogMsg
    = LogEval (FuseNoFuses Reg) [Int] Quality
    | LogEnd
    | LogNoOutput String
    deriving (Show)


runOn :: Config -> FilePath -> [String] -> IO ()
runOn config hlo_opt hlo_opt_args = do
    createDirectoryIfMissing True opt_logs

    base_env <- getEnvironment

    call_opt "forward-pass" $ ("XLA_RPOF_FORWARD_FILE", graph_dump_file) : base_env

    comp_list <- parseGraphs <$> readFile graph_dump_file

    let available_comp_text = "Available computations are: " ++ show (fst <$> comp_list)

    let (compname, graph) = case configCompName config of
            Just name -> case find ((name ==) . fst) comp_list of
                Just x  -> x
                Nothing -> error $ "No such computation! " ++ available_comp_text
            Nothing -> case comp_list of
                [x] -> x
                []  -> error "Module contains no computations"
                _   -> error $ "Module contains more than 1 computation. Please pick one. " ++ available_comp_text


    let num_edges = length $ G.getEdges graph
    putStrLn $ "Read computation '" ++ compname ++ "' with " ++ show num_edges ++ " edges"

    let graph' = case configDropout config of
            Just ratio -> dropout (configDropoutPolicy config) ratio graph
            Nothing    -> graph

    let num_edges' = length $ G.getEdges graph'
    putStrLn $ "Working with " ++ show num_edges' ++ " edges"

    let !total_eval_count =
            let compute  = recPart fuse_into_all thread_budget rng_gen merge eval_eval_c graph'
                (_, res) = runCounterM compute
            in res

    if configOnlyCountEvals config
        then do
            let eval_rate = configEvalRate config
            let time_per_eval = secondsToNominalDiffTime $ fromRational $ toRational $ 1 / eval_rate
            let total_time = time_per_eval * fromIntegral total_eval_count
            putStrLn $ "Number of evaluations: " ++ show total_eval_count
            putStrLn $ "Time to compute: " ++ humanReadableDuration total_time ++ " (assuming eval rate of " ++ show eval_rate ++ "e/s)"
        else do
            eval_counter <- newTVarIO 0
            log_channel  <- newChan

            let compute = recPart fuse_into_all thread_budget rng_gen merge (eval eval_counter base_env log_channel compname) graph'

            start_time <- getCurrentTime
            (fnf, baseline, quality) <- withAsync (log_thread log_channel) $ \logger ->
                withAsync (update_thread eval_counter total_eval_count start_time) $ \_ -> do
                    res <- compute

                    let unique  = newUnique
                    let unique' = next unique

                    baseline     <- eval eval_counter base_env log_channel compname unique  emptyFnf
                    this_quality <- eval eval_counter base_env log_channel compname unique' res

                    writeChan log_channel LogEnd
                    wait logger
                    return (res, baseline, this_quality)
            putStrLn $ "Quality " ++ show quality ++ " (" ++ show baseline ++ "): " ++ show fnf

            let final_output = workdir ++ "/optimal-fnf"
            writeFile final_output $ serializeFNF compname $ first reverse fnf

            putStrLn $ "Final output in " ++ final_output


    where
        -- extracting config variables
        thread_budget = configThreadBudget config
        workdir       = configWorkingDir   config
        fuse_into_all = configGraphFuseAll config

        update_thread :: TVar Int -> Int -> UTCTime -> IO ()
        update_thread counter total_evals start_time = go
            where
                go :: IO ()
                go = do
                    threadDelay $ configEtaInterval config
                    time_now <- getCurrentTime
                    counter_now <- readTVarIO counter
                    let duration = time_now `diffUTCTime` start_time
                    let eval_rate = fromIntegral counter_now / nominalDiffTimeToSeconds duration
                    let time_remaining = duration / fromIntegral counter_now * fromIntegral (total_evals-counter_now)
                    putStrLn
                        $ "Running for "
                        ++ humanReadableDuration duration
                        ++ ": (" ++ show counter_now ++ "/" ++ show total_evals
                        ++ ": " ++ show eval_rate ++ "e/s) "
                        ++ "ETA " ++ humanReadableDuration time_remaining
                    go


        rng_gen :: StdGen
        rng_gen = mkStdGen 0xC0A71

        graph_dump_file :: FilePath
        graph_dump_file = workdir ++ "/graph"

        eval :: TVar Int -> [(String, String)] -> Chan LogMsg -> String -> Unique -> FuseNoFuses Reg -> IO Quality
        eval eval_counter base_env log_channel cname unique fnf = do

            let instr_file = workdir ++ "/fnf" ++ show unique
            let out_file   = workdir ++ "/force_out" ++ show unique
            writeFile instr_file $ serializeFNF cname $ first reverse  fnf

            retcode <- try $ call_opt ("eval-" ++ show unique)
                $ ("XLA_RPOF_FORCE_FILE"  , instr_file)
                : ("XLA_RPOF_QUALITY_FILE", out_file)
                : ("XLA_RPOF_COMPUTATION" , cname)
                : base_env

            case retcode of
                Left ex -> do
                    print (ex :: SomeException)
                    putStrLn "assuming 0 quality"
                    return 0
                Right () -> doesFileExist out_file >>= \case
                    True -> do
                        ev <- parseEval <$> readFile out_file
                        let quality = if evalValid ev then 1.0 / evalExecNanos ev else 0

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

        withLog :: FilePath -> (StdStream -> IO r) -> IO r
        withLog path fn = if configHloOptLog config
            then withFile path WriteMode (fn . UseHandle)
            else fn NoStream

        call_opt :: String -> [(String, String)] -> IO ()
        call_opt suffix opt_env =
            withLog opt_out $ \opt_out_hdl ->
            withLog opt_err $ \opt_err_hdl -> do
                let cp = create_process opt_out_hdl opt_err_hdl
                (_, _, _, process_handle) <- createProcess_ "call_opt" cp
                waitForProcess process_handle >>= \case
                    ExitSuccess -> return ()
                    ExitFailure e -> do
                        print cp
                        error $ "opt failed with exit code " ++ show e ++ ". Logs at " ++ opt_out ++ " & " ++ opt_err
            where
                create_process :: StdStream -> StdStream -> CreateProcess
                create_process opt_out_hdl opt_err_hdl = (proc hlo_opt $ hlo_opt_args ++ [configHloPath config])
                    { std_out = opt_out_hdl
                    , std_err = opt_err_hdl
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



humanReadableDuration :: NominalDiffTime -> String
humanReadableDuration t
    =  d_padded ++ "d "
    ++ h_padded ++ "h "
    ++ m_padded ++ "m "
    ++ s_padded ++ "s"
    where
        seconds :: Int
        seconds = round $ nominalDiffTimeToSeconds t
        minutes = seconds `div` 60
        hours   = minutes `div` 60
        days    = hours   `div` 24

        d_part = days
        h_part = hours   `mod` 24
        m_part = minutes `mod` 60
        s_part = seconds `mod` 60

        d_padded = show d_part
        h_padded = (if h_part < 10 then " " else "") ++ show h_part
        m_padded = (if m_part < 10 then " " else "") ++ show m_part
        s_padded = (if s_part < 10 then " " else "") ++ show s_part
