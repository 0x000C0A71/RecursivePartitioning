{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import qualified Options.Applicative as AP

import Control.Applicative ((<**>))
import Data.List           (sort)
import System.Directory    (makeAbsolute, removeFile)
import System.Environment  (lookupEnv, getEnvironment)
import System.Exit         (ExitCode(ExitFailure, ExitSuccess))
import System.Process      (StdStream (NoStream), createProcess_, waitForProcess, proc, CreateProcess (..))

-- XLA_RPOF_FORCE_FILE=/RecursivePartitioning/workdir/optimal-fnf /xla/bazel-bin/xla/tools/run_hlo_module --platform=CUDA /hlos/small.hlo

type Env = [(String, String)]

parseArgs :: IO (FilePath, Int, FilePath, FilePath)
parseArgs = AP.execParser opts
    where
        opts = AP.info (argParser <**> AP.helper)
            (  AP.fullDesc
            <> AP.progDesc "Run an optimized module to compare compimizations to baseline"
            )

        argParser :: AP.Parser (FilePath, Int, FilePath, FilePath)
        argParser = do
            module_path <- AP.strArgument
                (  AP.help "Path to the hlo module file"
                <> AP.metavar "MODULE"
                )
            fuses <- AP.strArgument
                (  AP.help "Path to the optimized fuses file"
                <> AP.metavar "FNF_FILE"
                )
            runs <- AP.option AP.auto
                (  AP.long "runs"
                <> AP.short 'r'
                <> AP.help "Number of runs per version"
                <> AP.showDefault
                <> AP.value 50
                <> AP.metavar "RUNS"
                )
            csv_path <- AP.strOption
                (  AP.long "output"
                <> AP.short 'o'
                <> AP.help "Path to put the results"
                <> AP.showDefault
                <> AP.value "results.csv"
                <> AP.metavar "RESULTS"
                )
            return (module_path, runs, csv_path, fuses)

main :: IO ()
main = do
    run_hlo <- lookupEnv "RUN_HLO_MODULE_PATH" >>= \case
        Just p  -> makeAbsolute p
        Nothing -> return "hlo_opt"

    base_env <- getEnvironment
    (module_path', runs, csv_path', fuses') <- parseArgs

    module_path <- makeAbsolute module_path'
    csv_path    <- makeAbsolute csv_path'
    fuses       <- makeAbsolute fuses'
    
    run run_hlo base_env module_path runs csv_path fuses

run :: String -> Env -> FilePath -> Int -> FilePath -> FilePath -> IO ()
run run_hlo base_env module_path runs csv_path fuses = do
    results <- call_n_times runs one
    let (baseline, ours) = unzip results

    putStrLn "Baseline:"
    show_info baseline
    putStrLn "Ours:"
    show_info ours

    let csv_lines = (:) "baseline,ours" $ (\(l, r) -> show l ++ ',' : show r) <$> results

    writeFile csv_path $ unlines csv_lines
    putStrLn $ "Written to " ++ show csv_path
    where
        call_n_times :: forall a. Int -> IO a -> IO [a]
        call_n_times top act = go 1
            where
                go :: Int -> IO [a]
                go n = do
                    putStrLn $ "Run " ++ show n ++ " of " ++ show top
                    v <- act
                    t <- if n < top then go (n+1) else return []
                    return $ v:t

        one :: IO (Double, Double)
        one = (,) <$> call_run base_env <*> call_run environ
            where
                environ = ("XLA_RPOF_FORCE_FILE", fuses) : base_env

        call_run :: Env -> IO Double
        call_run environ = do
            (_, _, _, process_handle) <- createProcess_ "call_run" create_process
            waitForProcess process_handle >>= \case
                ExitSuccess -> return ()
                ExitFailure e -> do
                    print create_process
                    error $ "run_hlo failed with exit code " ++ show e
            contents <- readFile tmp_file
            removeFile tmp_file
            let nanos :: Int = case lines contents of
                    [x] -> read x
                    []  -> error "File was empty"
                    _   -> error "File had multiple lines"

            return $ fromIntegral nanos / 1000000000
            where
                create_process :: CreateProcess
                create_process = (proc run_hlo ["--platform=CUDA", module_path])
                    { std_out = NoStream
                    , std_err = NoStream
                    , env     = Just $ ("XLA_RPOF_RUNTIME_FILE", tmp_file) : environ
                    }

                tmp_file :: FilePath
                tmp_file = "qual_out"

        show_info :: [Double] -> IO ()
        show_info values = do
            putStrLn $ concat $ (:) "Box values:" $ (' ' :) . show <$> box_vals
            putStrLn $ "Arithmetic Mean: " ++ show arith_mean
            putStrLn $ "Geometric Mean: " ++ show geo_mean
            where
                sorted = sort values
                count' = length values
                count  = fromIntegral count'

                indicies = round . (count *) <$> [0.25, 0.5, 0.75]
                lowest  = minimum values
                highest = maximum values
                [lower_quart, median, upper_quart] = extract indicies sorted
                box_vals = [lowest, lower_quart, median, upper_quart, highest]

                arith_mean = sum values / count
                geo_mean   = product values ** (1 / count)

                -- Indicies must be in order
                extract :: [Int] -> [a] -> [a]
                extract = go 0
                    where
                        go :: Int -> [Int] -> [a] -> [a]
                        go _ [] _ = []
                        go _ _ [] = []
                        go n (k:ks) (v:vs)
                            | n == k = v : go n ks (v:vs)
                        go n ks vs = go (n+1) ks vs
