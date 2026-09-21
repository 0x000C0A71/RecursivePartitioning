{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module RecPart
    ( recPart
    ) where

import qualified Graph as G
import qualified Unique
import Unique (Unique, newUnique)
import Types

import System.Random  (StdGen, RandomGen, uniformR, splitGen)
import Data.List      (minimumBy)
import Data.Ord       (comparing)
import Data.Bifunctor (first, second)

import qualified Data.Set as S
import qualified Data.Map as M

mergeMultiple
    :: forall v m . (Ord v, Monad m)
    => (Unique -> v -> v -> m (v, Unique))
    -> v
    -> Unique
    -> [v]
    -> G.Graph v
    -> m ([Fusion v], (G.Graph v, Unique))
mergeMultiple merge_fn from = go
    where
        go :: Unique -> [v] -> G.Graph v -> m ([Fusion v], (G.Graph v, Unique))
        go unique [] g = return ([], (g, unique))
        go unique (to:rest) g = do
            (new_name, unique') <- merge_fn unique from to
            let new_entry = (from, to, new_name)
            first (new_entry:) <$> go unique' rest (G.mergeEdge from to new_name g)


class Splitable a where
    primSplit :: Int -> a -> [a]

    {-# INLINE split #-}
    split :: Int -> a -> [a]
    split 0 _ = []
    split 1 v = [v]
    split n v = primSplit n v

instance Splitable StdGen where
    primSplit = go []
        where
            go :: [StdGen] -> Int -> StdGen -> [StdGen]
            go gens 1 gen = gen:gens
            go gens n gen = go (a:gens) (n-1) b
                where
                    (a, b) = splitGen gen

instance Splitable Unique where
    primSplit = Unique.split

instance Splitable Budget where
    primSplit n v = replicate n $ v / fromIntegral n

($:) :: Splitable a => [a -> b] -> a -> [b]
fns $: s = zipWith ($) fns $ split n s
    where
        n = length fns


-- | Perform recursive partitioning on a graph
--
-- Searches for the optimal set of edges to fuse such that a
-- quality metric returned by the passed eval function is maximized
recPart :: forall v m q . (Ord v, Ord q, Monad m, MonadPar m)
    => Bool   -- ^ Fuse into all successors
    -> Budget -- ^ Parallel bifurcation budget
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
{-# SPECIALIZE recPart :: Bool -> Budget -> StdGen -> (Unique -> Reg -> Reg -> IO (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> IO Quality) -> G.Graph Reg -> IO (FuseNoFuses Reg) #-}
{-# SPECIALIZE recPart :: Bool -> Budget -> StdGen -> (Unique -> Reg -> Reg -> CounterM (Reg, Unique)) -> (Unique -> FuseNoFuses Reg -> CounterM Int) -> G.Graph Reg -> CounterM (FuseNoFuses Reg) #-}
recPart fuse_into_all bud gen merge eval root_graph = case G.getSubgraphs root_graph of
        []  -> error "No graph?"
        [g] -> go_root g
        gs  -> combineSets emptyFnf <$> mapM go_root gs
    where
        go_root :: G.Graph v -> m (FuseNoFuses v)
        go_root g = snd <$> go emptyFnf g bud gen newUnique newUnique

        go :: FuseNoFuses v -> G.Graph v -> Budget -> StdGen -> Unique -> Unique -> m (q, FuseNoFuses v)
        go !f !g !budget !rng !merge_u !eval_u = case edge_policy rng g of
            Nothing -> (,f) <$> eval eval_u f
            Just (from, to, rng') -> do
                let succs = if fuse_into_all then G.getSuccessors g from else [to]

                (merged_fusions, (merged_graph, merge_u')) <- mergeMultiple merge from merge_u succs g
                let merged_fnf = first (merged_fusions ++) f

                let no_fuse_edges = (from,) <$> succs
                let split_fnf     = second (S.union $ S.fromList no_fuse_edges) f
                let split_graph   = foldr (uncurry G.removeEdge) g no_fuse_edges

                let merged_components = G.getSubgraphs merged_graph
                let split_components  = G.getSubgraphs split_graph
                let merged_c_count    = length merged_components
                let split_c_count     = length split_components

                let branches
                        =  fmap (merged_fnf,) merged_components
                        ++ fmap (split_fnf ,) split_components
                let branch_count = merged_c_count + split_c_count

                let eval_u_merged = eval_u
                let eval_u_split  = Unique.next eval_u_merged
                let eval_u_rec    = Unique.next eval_u_split

                let budgets =
                        let eval_estimates  = (exponential_base^) . length . G.getEdges <$> merged_components ++ split_components
                            eval_estimate_t = sum eval_estimates
                        in (*budget) . (/eval_estimate_t) <$> eval_estimates

                let acts = zipWith ($) (uncurry go <$> branches) budgets $: rng' $: merge_u' $: eval_u_rec

                results <- if ceiling budget >= branch_count
                    then parList acts
                    else sequence acts
                let (merged_results, split_results) = splitAt merged_c_count results

                merged_scored <- scoreOutcome eval_u_merged merged_fnf merged_results
                split_scored  <- scoreOutcome eval_u_split  split_fnf  split_results

                return $ case fst split_scored `compare` fst merged_scored of
                    GT -> split_scored
                    LT -> merged_scored
                    EQ -> if length (fst $ snd merged_scored) < length (fst $ snd split_scored)
                        then merged_scored
                        else split_scored

        -- | Tunable parameter to control how budget is distributed among recursive calls
        exponential_base :: Double
        exponential_base = 1.5

        scoreOutcome :: Unique -> FuseNoFuses v -> [(q, FuseNoFuses v)] -> m (q, FuseNoFuses v)
        scoreOutcome eu base []  = (,base) <$> eval eu base
        scoreOutcome _  _    [r] = return r
        scoreOutcome eu base rs  = (,sets) <$> eval eu sets
            where
                sets = combineSets base (map snd rs)

        combineSets :: FuseNoFuses v -> [FuseNoFuses v] -> FuseNoFuses v
        combineSets (base_fs, _) sets =
            ( concatMap (stripBase . fst) sets ++ base_fs
            , S.unions $ snd <$> sets
            )
            where
                base_len = length base_fs
                stripBase fs = take (length fs - base_len) fs

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

