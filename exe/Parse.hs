{-# LANGUAGE ScopedTypeVariables #-}

module Parse
    ( parseGraphs
    , serializeFNF
    , parseEval
    , parseArgs
    ) where

import Types
import qualified Data.Map as M
import qualified Graph as G


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


type ParserState = (String, Reg, M.Map String (G.Graph Reg))

parseGraphs :: String ->[(String, G.Graph Reg)]
parseGraphs
    = M.toList
    . (\(_,_,v) -> v)
    . flip (foldl (flip (.)) id . fmap one_line . lines) (undefined, undefined, M.empty)
    where
        one_line :: String -> ParserState -> ParserState
        one_line [] k = k
        one_line ('!':rest) (_   , _ , graphs) = (rest, undefined, M.insert rest G.empty graphs)
        one_line ('%':rest) (comp, _ , graphs) = (comp, OrigReg $ head $ words rest, graphs)
        one_line ('$':rest) (comp, to, graphs) = (comp, to, M.adjust (G.addEdge from to) comp graphs)
            where
                from = OrigReg $ head $ words rest
        one_line (c:_) _ = error $ "Malformed graph dump: Line starting with " ++ show c


parseEval :: String -> Eval
parseEval contents = Eval
    { evalLeafInstrs   = read leaf_instrs
    , evalNumKernels   = read num_kernels
    , evalNumLaunches  = read num_launches
    , evalBytesRead    = read bytes_read
    , evalBytesWritten = read bytes_written
    , evalFlops        = read flops
    , evalExecNanos    = read exec_nanos
    }
    where
        [leaf_instrs, num_kernels, num_launches, bytes_read, bytes_written, flops, exec_nanos] = lines contents


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
        go fp ("--count-only"                 :rest) = (go fp rest) { configOnlyCountEvals = True          }
        go fp ("--log-hlo-opt"                :rest) = (go fp rest) { configHloOptLog      = True          }
        go fp ("--fuse-all-consumers"         :rest) = (go fp rest) { configGraphFuseAll   = True          }

        go Nothing (path:rest) = go (Just path) rest
        go (Just _) (second:_) = error $ "Cannot pass multiple hlo modules '" ++ second ++ "'"
