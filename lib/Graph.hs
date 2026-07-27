{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Graph
    ( Graph()
    , empty
    , addEdge, removeEdge
    , mergeEdge
    , getSubgraphs
    , getEdges
    , getSuccessors
    , getPredecessors
    , getVertices

    , dbgShow
    , dbgVerify
    ) where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Bifunctor

import Data.Map ((!))

-- | Graph type
-- Directed graph type. Maps from vertex `v` to `(next, previous)`
-- where:
-- - `next`:     are all vertices k such that the directed edge `v` -> `k` is part of the graph
-- - `previous`: are all vertices k such that the directed edge `k` -> `v` is part of the graph
--
-- So it encodes, for each vertex, what it's (successors, predecessors) are
newtype Graph v = Graph (M.Map v (S.Set v, S.Set v))

empty :: Graph v
empty = Graph M.empty

-- | `addEdge k v g` returns g with the added edge `k` -> `v`
addEdge :: forall v . Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) = Graph $ (M.alter add_from to . M.alter add_to from) g
    where
        add_from :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        add_from = Just . second (S.insert from) . fromMaybe (S.empty, S.empty)

        add_to :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        add_to = Just . first (S.insert to) . fromMaybe (S.empty, S.empty)

-- | `removeEdge k v g` returns g with the edge `k` -> `v` removed
-- If the edge does not exist in `g`, `g` is returned unchanged
removeEdge :: forall v . Ord v => v -> v -> Graph v -> Graph v
removeEdge from to (Graph g) = Graph $ (M.alter remove_from to . M.alter remove_to from) g
    where
        remove_from :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        remove_from = del_if_empty . second (S.delete from) . fromMaybe (S.empty, S.empty)

        remove_to :: Maybe (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        remove_to = del_if_empty . first (S.delete to) . fromMaybe (S.empty, S.empty)

        del_if_empty :: (S.Set v, S.Set v) -> Maybe (S.Set v, S.Set v)
        del_if_empty (ss, sp) = if S.null ss && S.null sp
            then Nothing
            else Just (ss, sp)


-- | `mergeEdge k v m g` returns `g` with the edge `k` -> `v` "merged" into `m`
-- After, `v` will no longer exist and will be replaced with `m`
-- The connection from `k` to `v` will be eliminated, as `m` is supposed to express both
-- `k` and `v`. As such `m` inherits all predecessors from `v` minus `k`, but also all
-- predecessors of `k`. Successors remain unchanged. The edge `k` -> `v` is deleted. If this
-- results in `k` having no remaining successors, it is also deleted. If, however, any other
-- vertices remain for which `k` is a predecessor, `k` is kept
mergeEdge :: forall v . Ord v => v -> v -> v -> Graph v -> Graph v
mergeEdge from to merged (Graph g) = Graph $ foldl (flip (.)) id mods g
    where
        (from_succs, from_preds) = g ! from
        (to_succs  , to_preds  ) = g ! to

        to_preds' = S.union from_preds $ S.delete from to_preds

        from_succs' = S.delete to from_succs

        mods = (M.adjust (first  (S.insert merged . S.delete to)) <$> S.toList to_preds')
            ++ (M.adjust (second (S.insert merged . S.delete to)) <$> S.toList to_succs )
            ++ (if S.null from_succs' then
                       (M.adjust (first  (S.delete from)) <$> S.toList from_preds )
                    ++ (M.adjust (second (S.delete from)) <$> S.toList from_succs')
                    ++ [M.delete from]
                else [M.insert from (from_succs', from_preds)])
            ++ [M.delete to, M.insert merged (to_succs, to_preds'), M.alter remove_if_empty merged]

        remove_if_empty Nothing = error "merged should exist"
        remove_if_empty (Just (ss, sp)) = if S.null ss && S.null sp
            then Nothing
            else Just (ss, sp)


-- | Given a graph, this function returns all disconnected subgraphs
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



-- | Given a graph, this function returns all edges contained in the graph
getEdges :: forall v . Ord v => Graph v -> [(v, v)]
getEdges (Graph graph) = do
    (from, (tos, _)) <- M.assocs graph
    to <- S.toList tos
    return (from, to)

-- | Returns the successors for a given vertex
getSuccessors :: forall v . Ord v => Graph v -> v -> [v]
getSuccessors (Graph graph) = maybe [] (S.toList . fst) . flip M.lookup graph


-- | Returns the predecessors for a given vertex
getPredecessors :: forall v . Ord v => Graph v -> v -> [v]
getPredecessors (Graph graph) = maybe [] (S.toList . snd) . flip M.lookup graph

-- | Returns all known vertices
getVertices :: forall v . Graph v -> [v]
getVertices (Graph g) = M.keys g




-- | Debug function to print the internal structure of the graph
--
-- Intended for debuging, may not be exported in the future
dbgShow :: forall v . (Ord v, Show v) => Graph v -> String
dbgShow (Graph m) = unlines $ for_each <$> M.toList m
    where
        for_each :: (v, (S.Set v, S.Set v)) -> String
        for_each (el, (to, from)) = unlines $ show el : tos ++ frs
            where
                tos = ("-> " ++) . show <$> S.toList to
                frs = ("<- " ++) . show <$> S.toList from

-- | Check the invariant, that for an edge `k` -> `v`,
-- `k` must be recorded as a predecessor of `v` and
-- `v` must be recorded as a successor of `k`
--
-- Function either returns `True` or an error informing
-- about the broken invariant
--
-- Intended for debuging, may not be exported in the future
dbgVerify :: forall v . (Ord v, Show v) => Graph v -> Bool
dbgVerify (Graph m) = all verify_one $ M.toList m
    where
        verify_one :: (v, (S.Set v, S.Set v)) -> Bool
        verify_one (v, (to, from)) =
            all tofn (S.toList to) &&
            all frfn (S.toList from)
            where
                tofn = mb ("to set wrong for "   ++ show v) . snd . (m M.!)
                frfn = mb ("from set wrong for " ++ show v) . fst . (m M.!)

                mb msg s = S.member v s || error msg












