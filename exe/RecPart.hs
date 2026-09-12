{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module RecPart
    ( recPart
    ) where

import qualified Graph as G
import Unique
import Types

import System.Random  (StdGen, RandomGen, uniformR)
import Data.List      (minimumBy)
import Data.Ord       (comparing)
import Data.Bifunctor (first, second)

import qualified Data.Set      as S
import qualified Data.Map      as M
import qualified System.Random as R


{- START AI-GENERATED CODE -}

-- | Fuse a producer `from` into every one of its consumers (successors).
--
-- Each outgoing edge `from -> s` is merged into its own fresh vertex, so the
-- producer is cloned into each consumer and then disappears. Threads the merge
-- function and the unique source; returns the accumulated fusion triples, the
-- transformed graph, and the updated unique source.
fuseAll :: (Ord v, Monad m)
        => (Unique -> v -> v -> m (v, Unique))
        -> Unique
        -> v          -- ^ producer
        -> [v]        -- ^ its consumers (successors)
        -> G.Graph v
        -> m ([Fusion v], G.Graph v, Unique)
fuseAll merge u from succs g = go u succs g
    where
        go u [] g = return ([], g, u)
        go u (s:ss) g = do
            (name, u') <- merge u from s
            (rest, g', u'') <- go u' ss (G.mergeEdge from s name g)
            return ((from, s, name) : rest, g', u'')

{- END AI-GENERATED CODE -}


-- | Perform recursive partitioning on a graph
--
-- Searches for the optimal set of edges to fuse such that a
-- quality metric returned by the passed eval function is maximized
recPart :: forall v m q . (Ord v, Ord q, Monad m, MonadPar m)
    => Budget -- ^ Parallel bifurcation budget
    -> StdGen -- ^ Random number generator.
    -> (Unique -> v -> v -> m (v, Unique))
    -- ^ Merge function.
    -- When merging an edge of the graph, this function
    -- is used to generate the newly created "merged"
    -- vertex. A `Unique` ID source is provided which
    -- must be returned, both if stepped using `next`
    -- or if left unchanged.
    -> (Unique -> FuseNoFuses v -> m q)
    -- ^ Eval function
    -- When evaluating a leaf, this function is used to get
    -- a "quality" metric. It is provided with a `Unique` ID
    -- source, but it is not allowed to step it (as indicated
    -- by it not returning a `Unique`)
    -> G.Graph v -- ^ Graph to run the search on
    -> m (FuseNoFuses v)
--{-# SPECIALIZE recPart @Reg @IO @Quality #-}
--{-# SPECIALIZE recPart @Reg @CounterM @Int #-}
{-# SPECIALIZE recPart ::  Budget -> StdGen -> (Unique -> Reg -> Reg -> IO (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> IO Quality) -> G.Graph Reg -> IO (FuseNoFuses Reg) #-}
{-# SPECIALIZE recPart ::  Budget -> StdGen -> (Unique -> Reg -> Reg -> CounterM (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> CounterM Int) -> G.Graph Reg -> CounterM (FuseNoFuses Reg) #-}
recPart bud gen merge eval = fmap snd . go bud gen ([], S.empty) newUnique newUnique
    where
        go :: Budget -> StdGen -> FuseNoFuses v -> Unique -> Unique -> G.Graph v -> m (q, FuseNoFuses v)
        go !budget !rng !f !merge_u !eval_u !g = case edge_policy rng g of
            Nothing -> (,f) <$> eval eval_u f
            Just (from, _, rng') -> do
                {- START AI-GENERATED CODE -}
                let succs = G.getSuccessors g from

                (merged_triples, merged_graph, merge_u') <- fuseAll merge merge_u from succs g

                let no_fuse_edges = [(from, s) | s <- succs]
                let with_merged = first (merged_triples ++) f
                let with_split = second (S.union (S.fromList no_fuse_edges)) f
                let split_graph = foldr (uncurry G.removeEdge) g no_fuse_edges
                {- END AI-GENERATED CODE -}

                let eval_u' = next eval_u
                let (eval_u1, eval_u2) = split2 eval_u'


                let (rng1, rng2) = R.split rng'
                let merged_act = go (budget/2) rng1 with_merged merge_u' eval_u1 merged_graph
                ((merged_quality, merged_sets), (split_quality , split_sets)) <- case G.getSubgraphs split_graph of
                    []  -> do
                        mres <- merged_act
                        quality <- eval eval_u with_split
                        return (mres, (quality, f))
                    [x] ->
                        let unmerged_act = go (budget/2) rng2 with_split merge_u' eval_u2 x
                        in if budget > 1
                            then par2 (merged_act, unmerged_act)
                            else (,) <$> merged_act <*> unmerged_act
                    [x1, x2] -> do
                        let (eval_us1,  eval_us2 ) = split2 eval_u2
                        let (merge_us1, merge_us2) = split2 merge_u'
                        let go' = go (budget/4) rng2 with_split
                        let act1 = go' merge_us1 eval_us1 x1
                        let act2 = go' merge_us2 eval_us2 x2
                        (mres, (_, (fuse1, nfuse1)), (_, (fuse2, nfuse2))) <- case (budget > 1, budget > 2) of
                            (True , True ) -> par3 (merged_act, act1, act2)
                            (True , False) -> (\(a, (b, c)) -> (a, b, c)) <$> par2 (merged_act, (,) <$> act1 <*> act2)
                            (False, False) -> do
                                m  <- merged_act
                                s1 <- act1
                                s2 <- act2
                                return (m, s1, s2)
                            (False, True ) -> error "Not possible by transitivity of (>)"
                        let sets = (fuse1 ++ fuse2, nfuse1 `S.union` nfuse2)
                        quality <- eval eval_u sets
                        return (mres, (quality, sets))
                    {- START AI-GENERATED CODE -}
                    xs -> do
                        let n = length xs
                            child_budget = budget / (2 * fromIntegral n)
                            merge_us = split n merge_u'
                            eval_us = split n eval_u2
                            acts = [ go child_budget rng2 with_split mu eu x | (mu, eu, x) <- zip3 merge_us eval_us xs ]
                        (mres, split_results) <-
                            if budget > 1
                                then par2 (merged_act, parList acts)
                                else (,) <$> merged_act <*> sequence acts
                        let sets = ( concat [fs | (_, (fs, _)) <- split_results]
                                   , S.unions [ns | (_, (_, ns)) <- split_results] )
                        quality <- eval eval_u sets
                        return (mres, (quality, sets))
                    {- END AI-GENERATED CODE -}
                return $ if split_quality > merged_quality
                    then (split_quality, split_sets)
                    else (merged_quality, merged_sets)

        edge_policy = edge_policy_mincut


        {- START AI-GENERATED CODE -}

        -- | Pick a bridge if one exists (preferring the most balanced), else
        -- fall back to the mass heuristic.
        edge_policy_bridge :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_bridge rng g = case G.getEdges g of
            []    -> Nothing
            _     -> let (x, y) = maybe (massEdge g) id (bestBridge g) in Just (x, y, rng)

        -- | Pick a bridge if one exists; else if there is a small (width <= 3)
        -- and balanced min-cut, cut across it; else fall back to mass.
        edge_policy_mincut :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_mincut rng g = case G.getEdges g of
            []    -> Nothing
            _     -> let (x, y) = maybe (neckEdge g) id (bestBridge g) in Just (x, y, rng)
          where
            neckEdge g = case G.minCut g of
                (l, side) | l <= 3 && balancedCut g side -> crossingEdge g side
                _ -> massEdge g

        -- Most balanced bridge (if any): the bridge whose removal splits the
        -- graph into two most-equal halves, measured by edge count.
        bestBridge :: G.Graph v -> Maybe (v, v)
        bestBridge g = case G.bridges g of
            [] -> Nothing
            bs -> Just $ minimumBy (comparing balance) bs
          where
            balance (u, w) =
                case map (length . G.getEdges) (G.getSubgraphs (G.removeEdge u w g)) of
                    [n]    -> n                     -- leaf bridge: one side is empty
                    [a, b] -> abs (a - b)
                    _      -> error "bridge must split into 1 or 2 components"

        -- Is this cut (given one side) balanced enough to be worth cutting?
        balancedCut :: G.Graph v -> S.Set v -> Bool
        balancedCut g side =
            let n = S.size side
                total = length (G.getVertices g)
            in n >= 2 && (total - n) >= 2

        -- An edge crossing the cut (one endpoint in `side`, the other not).
        crossingEdge :: G.Graph v -> S.Set v -> (v, v)
        crossingEdge g side =
            head [ (u, w) | (u, w) <- G.getEdges g, S.member u side /= S.member w side ]


        -- The mass heuristic's edge pick, extracted for reuse.
        massEdge :: G.Graph v -> (v, v)
        massEdge g = minimumBy (comparing evalEdge) (G.getEdges g)
          where
            (intos, outs) = G.getMassMaps g
            evalEdge (f, t) = abs $ (intos M.! f) - (outs M.! t)

        {- END AI-GENERATED CODE -}

        -- | The whole algorithm "chops" one edge away each iteration,
        -- hoping to "chop" the graph in half. This function "aims" the
        -- "axe". The main goal is to "chop" the graph in two as fast as
        -- possible because that's where the speedup comes from.
        -- `pick_edges` uses the "mass" heuristic, trying to find the edge
        -- which has the lowest "mass"-difference (upstream "mass" of
        -- producer minus downstream "mass" of consumer). While this "mass"
        -- metric is relatively bad for getting equally-sized halves, it is
        -- rather quick to compute, and the idea is, to instead of finding
        -- the perfect place to "chop", we just "roughly aim for the center",
        -- and will probably get two parts rather quickly.
        edge_policy_mass :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_mass rng g = case G.getEdges g of
            []    -> Nothing
            edges ->
                let (x, y) = minimumWith evalEdge edges
                in Just (x, y, rng)
            where
                (intos, outs) = G.getMassMaps g

                evalEdge (f, t) = abs $ (intos M.! f) - (outs M.! t)

                minimumWith :: (Ord b, Foldable t) => (a -> b) -> t a -> a
                minimumWith fn = minimumBy $ \l r -> compare (fn l) (fn r)

        edge_policy_random :: RandomGen g => g -> G.Graph v -> Maybe (v, v, g)
        edge_policy_random rng g = case G.getEdges g of
            []    -> Nothing
            edges ->
                let
                    edge_count = length edges
                    (idx, rng') = uniformR (0, edge_count-1) rng
                    (x, y) = edges !! idx
                in Just (x, y, rng')

