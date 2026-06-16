{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Graph as G
import qualified Data.Set as S

import Data.Bifunctor
import Control.Parallel

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

recPart :: forall v m . (Ord v, Ord m) => (v -> v -> v) -> (G.Graph v -> m) -> (v -> v -> m -> m -> m) -> m -> G.Graph v -> (S.Set (v, v), S.Set (v, v))
recPart merge eval metric_merge base_metric = snd . go
    where
        go :: G.Graph v -> (m, (S.Set (v, v), S.Set (v, v)))
        go !g = case pick_edge g of
            Just (from, to) ->
                let
                    (metric, sets, func) = mm `par` ms `pseq` selector
                        where
                            selector = if mm > ms then (mm, sm, first) else (ms, ss, second)

                    (mm, sm) = go $ G.mergeEdge from to (merge from to) g
                    (ms, ss) = case G.getSubgraphs $ G.removeEdge from to g of
                        [x] -> go x
                        [x, y] ->
                            let (xm, (xs1, xs2)) = go x
                                (ym, (ys1, ys2)) = go y
                                mmet = metric_merge from to xm ym
                            in (mmet, (xs1 `S.union` ys1, xs2 `S.union` ys2))
                        _ -> error "More than 2 subgraphs produced"
                in (metric, func (S.insert (from, to)) sets)
            Nothing -> (base_metric, (S.empty, S.empty))


        -- TODO: be more clever
        pick_edge :: G.Graph v -> Maybe (v, v)
        pick_edge g = case G.getEdges g of
            []    -> Nothing
            (e:_) -> Just e


