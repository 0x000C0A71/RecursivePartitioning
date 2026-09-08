{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
    , getMassMaps
    , getVerticesTopological
    , getEdgesTopological

    , minCut
    , bridges

    , dbgShow
    , dbgVerify
    ) where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Control.Monad (forM_, when)
import Control.Monad.State
import Data.List (foldl', maximumBy, delete)
import Data.Ord (comparing)

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

-- | ONLY WORKS FOR DAGS
-- For each vertex, computes a "mass" upstream and downstream of the vertex
-- e.g. upstream "mass" for a given vertex is the sum of the "masses" of all
-- its predecessors plus the number of edges by which those predecessors are
-- connected. (So sum of predecessor masses + number of incomming edges)
-- downstream "mass" works the same but reversed
--
-- Runs in O(nm) for n vertices and m edges
--
-- returns a map for upstream and downstream "mass" each of each vertex as
-- (<upstream map>, <downstream map>)
getMassMaps :: forall v . Ord v => Graph v -> (M.Map v Int, M.Map v Int)
getMassMaps (Graph m) = bimap eval eval monads
    where
        do_one :: Bool -> v -> State (M.Map v Int) Int
        do_one forward = go
            where
                fn = if forward then snd else fst

                go :: v -> State (M.Map v Int) Int
                go k = gets (M.lookup k) >>= \case
                    Just n  -> return n
                    Nothing -> do
                        let recs = fn $ m ! k
                        this <- S.fold (\neighbor prev -> (+) <$> prev <*> go neighbor) (return $ S.size recs) recs
                        modify $ M.insert k this
                        return this

        monads :: (State (M.Map v Int) Int, State (M.Map v Int) Int)
        monads = S.fold fld (return undefined, return undefined) $ M.keysSet m
            where
                fld k = bimap (>> do_one True k) (>> do_one False k)

        eval :: State (M.Map v Int) a -> M.Map v Int
        eval = flip execState M.empty

getVerticesTopological :: forall v . Ord v => Graph v -> [v]
getVerticesTopological (Graph g) = rev_list
    where
        go :: v -> State (S.Set v) [v]
        go k = gets (S.member k) >>= \case
            True -> return []
            False -> do
                modify $ S.insert k
                let preds = S.toList $ snd $ g M.! k
                l <- mconcat <$> mapM go preds
                return $ l ++ [k]

        all_verts = S.toList $ M.keysSet g

        rev_list = mconcat $ evalState (mapM go all_verts) S.empty

getEdgesTopological :: forall v . Ord v => Graph v -> [(v, v)]
getEdgesTopological g = getVerticesTopological g >>= get_incoming
    where
        get_incoming :: v -> [(v, v)]
        get_incoming k = (,k) <$> getPredecessors g k

-- TODO: At least add tests
{- START AI-GENERTED CODE -}

-- | All bridges (cut-edges) of the graph, treated as undirected.
-- A bridge is an edge whose removal disconnects the graph.
-- Tarjan's algorithm, runs in O(V + E).
bridges :: forall v . Ord v => Graph v -> [(v, v)]
bridges (Graph m) = go $ execState (mapM_ (dfs Nothing) (M.keys m)) initSt
    where
        neigh :: v -> [v]
        neigh v = S.toList (fst (m M.! v)) ++ S.toList (snd (m M.! v))

        -- Return the bridge in the graph's actual (directed) edge direction,
        -- since the DFS tree edge (v, w) may traverse a predecessor instead
        -- of a successor.
        dirEdge :: v -> v -> (v, v)
        dirEdge v w = if w `S.member` fst (m M.! v) then (v, w) else (w, v)

        initSt :: (M.Map v Int, M.Map v Int, Int, [(v, v)])
        initSt = (M.empty, M.empty, 0, [])

        go :: (M.Map v Int, M.Map v Int, Int, [(v, v)]) -> [(v, v)]
        go (_, _, _, b) = b

        dfs :: Maybe v -> v -> State (M.Map v Int, M.Map v Int, Int, [(v, v)]) ()
        dfs parent v = do
            (vis, _, _, _) <- get
            case M.lookup v vis of
                Just _  -> pure ()
                Nothing -> do
                    (vis, low, tm, br) <- get
                    put (M.insert v tm vis, M.insert v tm low, tm + 1, br)
                    forM_ (neigh v) $ \w -> when (Just w /= parent) $ do
                        (vis2, _, _, _) <- get
                        case M.lookup w vis2 of
                            Just wt -> do
                                (v3, l3, t3, b3) <- get
                                put (v3, M.adjust (min wt) v l3, t3, b3)
                            Nothing -> do
                                dfs (Just v) w
                                (v4, l4, t4, b4) <- get
                                let wLow  = l4 M.! w
                                    vDisc = v4 M.! v
                                put (v4, M.adjust (min wLow) v l4, t4
                                        , if wLow > vDisc then dirEdge v w : b4 else b4)


-- | Global minimum edge cut on the undirected view of the graph.
-- Returns (cut width, one side of a witness cut). Stoer-Wagner, O(V * E).
minCut :: forall v . Ord v => Graph v -> (Int, S.Set v)
minCut (Graph m)
    | M.null und = (0, S.empty)
    | otherwise  = go (maxBound :: Int, S.empty) w0 orig0 ids0
    where
        und :: M.Map v (S.Set v)
        und = M.map (\(s, p) -> S.union s p) m

        idList :: [(v, Int)]
        idList = zip (M.keys und) [0..]

        idOf :: v -> Int
        idOf x = M.fromList idList M.! x

        orig0 :: M.Map Int (S.Set v)
        orig0 = M.fromList [(i, S.singleton x) | (x, i) <- idList]

        w0 :: M.Map (Int, Int) Int
        w0 = M.fromList [ ((min i j, max i j), 1)
                        | (x, ns) <- M.toList und, y <- S.toList ns
                        , let i = idOf x, let j = idOf y, i < j ]

        ids0 :: [Int]
        ids0 = [0 .. M.size und - 1]

        weight :: M.Map (Int, Int) Int -> Int -> Int -> Int
        weight w a b = M.findWithDefault 0 (min a b, max a b) w

        go :: (Int, S.Set v) -> M.Map (Int, Int) Int -> M.Map Int (S.Set v) -> [Int] -> (Int, S.Set v)
        go best w orig ids
            | length ids <= 1 = best
            | otherwise =
                let (cutW, tId, sId) = phase w ids
                    best' = if cutW < fst best then (cutW, orig M.! tId) else best
                    (w', orig', ids') = contract w orig ids sId tId
                in go best' w' orig' ids'

        phase :: M.Map (Int, Int) Int -> [Int] -> (Int, Int, Int)
        phase w ids =
            let start = head ids
                added = mas w start (tail ids) [(start, 0)]
                (tId, cutW) = head added
                (sId, _)     = head (tail added)
            in (cutW, tId, sId)

        mas :: M.Map (Int, Int) Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
        mas _ _ [] acc = acc
        mas w cur rest acc =
            let (x, wx) = maximumBy (comparing snd)
                            [ (x, sum [weight w x a | (a, _) <- acc]) | x <- rest ]
            in mas w x (delete x rest) ((x, wx) : acc)

        contract :: M.Map (Int, Int) Int -> M.Map Int (S.Set v) -> [Int] -> Int -> Int
                 -> (M.Map (Int, Int) Int, M.Map Int (S.Set v), [Int])
        contract w orig ids s t =
            let merged = (orig M.! s) `S.union` (orig M.! t)
                orig'  = M.insert s merged (M.delete t orig)
                others = filter (\x -> x /= s && x /= t) ids
                w' = foldl' (mergeOne s t) (M.delete (min s t, max s t) w) others
                ids' = filter (/= t) ids
            in (w', orig', ids')
          where
            mergeOne s t acc x =
                let wNew  = weight w s x + weight w t x
                    keySx = (min s x, max s x)
                    keyTx = (min t x, max t x)
                in if wNew > 0
                     then M.insert keySx wNew (M.delete keyTx acc)
                     else M.delete keyTx (M.delete keySx acc)

{- END AI-GENERTED CODE -}


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












