{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

module Parse
    ( parseGraphs
    , serializeFNF
    , parseEval
    , parseArgs
    , getConfig
    ) where

import Types
import qualified Data.Map as M
import qualified Graph as G

import qualified Options.Applicative as AP

import Control.Applicative ((<**>))
import Data.Bifunctor      (second, first)
import Data.Maybe          (fromMaybe)
import GHC.Conc            (numCapabilities)

--(Reg, Reg, Reg)
serializeFNF :: String -> FuseNoFuses Reg -> String
serializeFNF cname (xs, _) = unlines $ do_one <$> xs
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


type ParserState = (String, Reg, M.Map String (Int, G.Graph Reg))

parseGraphs :: String -> [(String, (Int, G.Graph Reg))]
parseGraphs
    = M.toList
    . (\(_,_,v) -> v)
    . flip (foldl (flip (.)) id . fmap one_line . lines) (undefined, undefined, M.empty)
    where
        one_line :: String -> ParserState -> ParserState
        one_line [] k = k
        one_line ('!':rest) (_   , _ , graphs) = (rest, undefined, M.insert rest (0, G.empty) graphs)
        one_line ('%':rest) (comp, _ , graphs) = (comp, OrigReg $ head $ words rest, graphs)
        one_line ('$':rest) (comp, to, graphs) = (comp, to, M.adjust adj comp graphs)
            where
                (from':fusible:_) = words rest
                from = OrigReg from'

                adj = if read fusible then second $ G.addEdge from to else first (+1)

        one_line (c:_) _ = error $ "Malformed graph dump: Line starting with " ++ show c


parseEval :: String -> Eval
parseEval contents = Eval
    { evalValid     = read valid
    , evalExecNanos = read exec_nanos
    }
    where
        --[leaf_instrs, num_kernels, num_launches, bytes_read, bytes_written, flops, exec_nanos] = lines contents
        [valid, exec_nanos] = lines contents


argParser :: AP.Parser Config
argParser = do
    dropout <- AP.optional $ AP.option AP.auto
        (  AP.long "dropout"
        <> AP.help "How much of the graph to drop. Omit to disable"
        <> AP.metavar "FLOAT"
        )
    policy <- AP.option AP.auto
        (  AP.long "dropout-policy"
        <> AP.help ("How to remove part of the graph. One of: " ++ show [DropoutBeginning,DropoutCenter])
        <> AP.showDefault
        <> AP.value DropoutBeginning
        <> AP.metavar "POLICY"
        )
    eta_interval :: Double <- AP.option AP.auto
        (  AP.long "eta-interval"
        <> AP.help "Seconds between eta printing"
        <> AP.showDefault
        <> AP.value 20
        <> AP.metavar "SECONDS"
        )
    workdir <- AP.option AP.auto
        (  AP.long "working-directory"
        <> AP.help "Working directory to store temporary files"
        <> AP.showDefault
        <> AP.value "."
        <> AP.metavar "PATH"
        )
    threading <- AP.option AP.auto
        (  AP.long "thread-budget"
        <> AP.help "Continous 'threading budget' to limit green thread production. Defaults to 4x runtime capabilities"
        <> AP.showDefault
        <> AP.value (fromIntegral $ numCapabilities * 4)
        <> AP.metavar "FLOAT"
        )
    eval_rate <- AP.option AP.auto
        (  AP.long "estimate-eval-rate"
        <> AP.help "Eval rate (as reported by the ETAs) to estimate runtime in count-only mode"
        <> AP.showDefault
        <> AP.value 60
        <> AP.metavar "FLOAT"
        )
    comp <- AP.optional $ AP.strOption
        (  AP.long "computation"
        <> AP.help "The computation to optimize. If ommited will assume module only has one"
        <> AP.metavar "NAME"
        )
    count <- AP.switch
        (  AP.long "count-only"
        <> AP.short 'c'
        <> AP.help "Perform no optimization, only count leaf evaluations"
        )
    log_hlo <- AP.switch
        (  AP.long "log-hlo-opt"
        <> AP.short 'l'
        <> AP.help "Keep logs of the `hlo-opt` calls around. CAUTION this creates a LOT of data"
        )
    fuse_all <- AP.switch
        (  AP.long "fuse-all-consumers"
        <> AP.short 'a'
        <> AP.help "Fuse producers into all consumers and perform no partial fusions"
        )
    hlo_path <- AP.strArgument
        (  AP.help "Path to a .hlo module to optimize"
        <> AP.metavar "HLO_PATH"
        )
    opt_args <- AP.optional $ AP.some $ AP.strArgument
        (  AP.help "Additional arguments to pass to `hlo-opt`"
        <> AP.metavar "HLO_OPT_ARGS"
        )
    return Config
        { configDropout        = dropout
        , configDropoutPolicy  = policy
        , configEtaInterval    = round $ eta_interval * 1000000
        , configWorkingDir     = workdir
        , configThreadBudget   = threading
        , configEvalRate       = eval_rate
        , configCompName       = comp
        , configOnlyCountEvals = count
        , configHloOptLog      = log_hlo
        , configGraphFuseAll   = fuse_all
        , configHloPath        = hlo_path
        , configHloOptArgs     = fromMaybe [] opt_args
        }


getConfig :: IO Config
getConfig = AP.execParser opts
    where
        opts = AP.info (argParser <**> AP.helper)
            (  AP.fullDesc
            <> AP.progDesc "Optimize the operator fusion decisions of a passed hlo module"
            )

parseArgs :: [String] -> Config
parseArgs = go Nothing
    where
        go :: Maybe FilePath -> [String] -> Config

        go Nothing     [] = defaultConfig
        go (Just path) [] = defaultConfig { configHloPath = path }

        go fp ("--dropout"           :num     :rest) = (go fp rest) { configDropout        = Just $ read num }
        go fp ("--dropout-policy"    :policy  :rest) = (go fp rest) { configDropoutPolicy  = read policy   }
        go fp ("--eta-interval-us"   :interval:rest) = (go fp rest) { configEtaInterval    = read interval }
        go fp ("--working-directory" :work_dir:rest) = (go fp rest) { configWorkingDir     = work_dir      }
        go fp ("--thread-budget"     :budget  :rest) = (go fp rest) { configThreadBudget   = read budget   }
        go fp ("--estimate-eval-rate":rate    :rest) = (go fp rest) { configEvalRate       = read rate     }
        go fp ("--computation"       :compname:rest) = (go fp rest) { configCompName       = Just compname }
        go fp ("--count-only"                 :rest) = (go fp rest) { configOnlyCountEvals = True          }
        go fp ("--log-hlo-opt"                :rest) = (go fp rest) { configHloOptLog      = True          }
        go fp ("--fuse-all-consumers"         :rest) = (go fp rest) { configGraphFuseAll   = True          }

        go Nothing (path:rest) = go (Just path) rest
        go (Just _) (second:_) = error $ "Cannot pass multiple hlo modules '" ++ second ++ "'"
