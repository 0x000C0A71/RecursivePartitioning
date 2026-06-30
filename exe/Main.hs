{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Graph as G
import qualified Data.Set as S

import Data.Bifunctor
import Control.Parallel

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

recPart :: forall v m q . (Ord v, Ord q, Monad m) => (v -> v -> v) -> ((S.Set (v, v), S.Set (v, v)) -> m q) -> G.Graph v -> m (S.Set (v, v), S.Set (v, v))
recPart merge eval = fmap snd . go (S.empty, S.empty)
    where
        go :: (S.Set (v, v), S.Set (v, v)) -> G.Graph v -> m (q, (S.Set (v, v), S.Set (v, v)))
        go !f !g = case pick_edge g of
            Nothing -> (,f) <$> eval f
            Just (from, to) -> do
                let ins = S.insert (from, to)

                (merged_quality, merged_sets) <- go (first ins f) $ G.mergeEdge from to (merge from to) g
                (split_quality , split_sets ) <- case G.getSubgraphs $ G.removeEdge from to g of
                    [x] -> go (second ins f) x
                    xs -> do
                        rec_results <- go (second ins f) `mapM` xs
                        let sets = bimap S.unions S.unions $ unzip $ snd <$> rec_results
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

