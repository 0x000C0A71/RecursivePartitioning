{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Graph
    ( Graph()
    , empty
    , addEdge, removeEdge
    , mergeEdge
    , getSubgraphs
    , getEdges
    ) where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Bifunctor

import Data.Map ((!))


newtype Graph v = Graph (M.Map v (S.Set v, S.Set v))

empty :: Graph v
empty = Graph M.empty

addEdge :: forall v . Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) = Graph $ (M.alter add_from to . M.alter add_to from) g
    where
        add_from :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        add_from = Just . second (S.insert from) . fromMaybe (S.empty, S.empty)

        add_to :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        add_to = Just . first (S.insert to) . fromMaybe (S.empty, S.empty)


removeEdge :: forall v . Ord v => v -> v -> Graph v -> Graph v
removeEdge from to (Graph g) = Graph $ (M.alter remove_from to . M.alter remove_to from) g
    where
        remove_from :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        remove_from = Just . second (S.delete from) . fromMaybe (S.empty, S.empty)

        remove_to :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        remove_to = Just . first (S.delete to) . fromMaybe (S.empty, S.empty)

mergeEdge :: forall v . Ord v => v -> v -> v -> Graph v -> Graph v
mergeEdge from to merged (Graph g) = undefined

getSubgraphs :: forall v . Ord v => Graph v -> [Graph v]
getSubgraphs (Graph m) = collect $ M.keysSet m
    where
        collect :: S.Set v -> [Graph v]
        collect !remaining = if S.null remaining
            then []
            else Graph graphed : collect (S.difference remaining chunk)
            where
                chunk = go (S.findMin remaining) S.empty

                graphed = S.fold (\k -> M.insert k $ m ! k) M.empty chunk
                
                go :: v -> S.Set v -> S.Set v
                go !curr_elem !curr_sec
                    | S.member curr_elem curr_sec = curr_sec
                    | otherwise = S.fold go added next
                    where
                        added = S.insert curr_elem curr_sec
                        (t, f) = m ! curr_elem
                        next = S.union t f




getEdges :: forall v . Ord v => Graph v -> [(v, v)]
getEdges (Graph graph) = do
    (from, (tos, _)) <- M.assocs graph
    to <- S.toList tos
    return (from, to)
















