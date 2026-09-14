{-# LANGUAGE ScopedTypeVariables #-}

module Parse
    ( parseGraphs
    , serializeFNF
    , parseEval
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


parseEval :: String ->Eval
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
