
module Main where

import qualified Graph as G

import Data.List (sort)

import Test.HUnit
import Control.Exception (SomeException(), try, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (sortWith)
import Control.Monad (zipWithM_)

type Reg = String

assertEdges :: [(Reg, Reg)] -> G.Graph Reg -> Assertion
assertEdges expected = assertEqual "Edges" (sort expected) . sort . G.getEdges

assertVertices :: [Reg] -> G.Graph Reg -> Assertion
assertVertices expected = assertEqual "Vertices" (sort expected) . sort . G.getVertices

assertEdgesVerts :: [(Reg, Reg)] -> [Reg] -> G.Graph Reg -> Assertion
assertEdgesVerts e v g = assertEdges e g >> assertVertices v g

assertSuccs :: Reg -> [Reg] -> G.Graph Reg -> Assertion
assertSuccs k vs = assertEqual ("Succerssors of " ++ show k) (sort vs) . sort . flip G.getSuccessors k

assertPreds :: Reg -> [Reg] -> G.Graph Reg -> Assertion
assertPreds k vs = assertEqual ("Predecessors of " ++ show k) (sort vs) . sort . flip G.getPredecessors k

assertInvariants :: G.Graph Reg -> Assertion
assertInvariants g = case unsafePerformIO (try $ evaluate $ G.dbgVerify g :: IO (Either SomeException Bool)) of
    Left  _     -> assertFailure "Invariants broken"
    Right True  -> assertBool undefined True
    Right False -> assertFailure "dbgVerify returned False"

assertSubgraphVerts :: [[Reg]] -> G.Graph Reg -> Assertion
assertSubgraphVerts verts g = do
    assertEqual "Subgraph count" (length verts) (length subgraphs)
    zipWithM_ assertVertices sorted_verts sorted_subgraphs
    where
        subgraphs = G.getSubgraphs g
        sorted_verts = sortWith length verts
        sorted_subgraphs = sortWith (length . G.getVertices) subgraphs

assertSubgraphInvariants :: G.Graph Reg -> Assertion
assertSubgraphInvariants = mapM_ assertInvariants . G.getSubgraphs

main :: IO Counts
main = runTestTT allTests


graphTest :: String -> [G.Graph Reg -> G.Graph Reg] -> [G.Graph Reg -> Assertion] -> Test
graphTest test_name funcs tests = TestLabel test_name $ TestCase $ mapM_ ($ graph) $ tests ++ [assertInvariants]
    where
        graph = foldl (flip ($)) G.empty funcs



allTests :: Test
allTests = TestList
    [ empty
    , addEdge
    , removeEdge
    , mergeEdge
    , getSubgraphs
    , getSuccessors
    , getPredecessors
    ]
    where
        empty = TestLabel "empty" $ TestList
            [ graphTest "No edges" []
                [ assertEdgesVerts [] []
                ]
            ]

        addEdge = TestLabel "addEdge" $ TestList
            [ graphTest "sanity check"
                [ G.addEdge "a" "b"
                ]
                [ assertEdgesVerts [("a", "b")] ["a", "b"]
                , assertSuccs "a" ["b"]
                , assertPreds "b" ["a"]
                , assertSuccs "b" []
                , assertPreds "a" []
                ]
            , graphTest "duplicate idempotence"
                [ G.addEdge "a" "b"
                , G.addEdge "a" "b"
                ]
                [ assertEdgesVerts [("a", "b")] ["a", "b"]
                , assertSuccs "a" ["b"]
                , assertPreds "b" ["a"]
                , assertSuccs "b" []
                , assertPreds "a" []
                ]
            , graphTest "multiple successors"
                [ G.addEdge "a" "b"
                , G.addEdge "a" "c"
                ]
                [ assertEdgesVerts [("a", "b"), ("a", "c")] ["a", "b", "c"]
                , assertSuccs "a" ["b", "c"]
                , assertSuccs "b" []
                , assertSuccs "c" []

                , assertPreds "a" []
                , assertPreds "b" ["a"]
                , assertPreds "c" ["a"]
                ]
            , graphTest "multiple predecessors"
                [ G.addEdge "a" "b"
                , G.addEdge "c" "b"
                ]
                [ assertEdgesVerts [("a", "b"), ("c", "b")] ["a", "b", "c"]
                , assertSuccs "a" ["b"]
                , assertSuccs "b" []
                , assertSuccs "c" ["b"]

                , assertPreds "a" []
                , assertPreds "b" ["a", "c"]
                , assertPreds "c" []
                ]
            , graphTest "chain"
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                ]
                [ assertEdgesVerts [("a", "b"), ("b", "c")] ["a", "b", "c"]
                , assertSuccs "a" ["b"]
                , assertSuccs "b" ["c"]
                , assertSuccs "c" []

                , assertPreds "a" []
                , assertPreds "b" ["a"]
                , assertPreds "c" ["b"]
                ]
            , graphTest "reflexive"
                [ G.addEdge "a" "a"
                ]
                [ assertEdgesVerts [("a", "a")] ["a"]
                , assertSuccs "a" ["a"]
                , assertPreds "a" ["a"]
                ]
            ]

        removeEdge = TestLabel "removeEdge" $ TestList
            [ graphTest "sanity check"
                [ G.addEdge "a" "b"
                , G.removeEdge "a" "b"
                ]
                [ assertEdgesVerts [] []
                , assertSuccs "a" []
                , assertSuccs "b" []
                , assertPreds "a" []
                , assertPreds "b" []
                ]
            , graphTest "non-existant edge"
                [ G.removeEdge "a" "b"
                ]
                [ assertEdgesVerts [] []
                , assertSuccs "a" []
                , assertSuccs "b" []
                , assertPreds "a" []
                , assertPreds "b" []
                ]
            , graphTest "deletion from multi-edge graph"
                [ G.addEdge "a" "b"
                , G.addEdge "a" "c"
                , G.removeEdge "a" "b"
                ]
                [ assertEdgesVerts [("a", "c")] ["a", "c"]
                , assertSuccs "a" ["c"]
                , assertSuccs "b" []
                , assertSuccs "c" []
                , assertPreds "a" []
                , assertPreds "b" []
                , assertPreds "c" ["a"]
                ]
            ]

        mergeEdge = TestLabel "mergeEdge" $ TestList
            [ graphTest "sanity check"
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                , G.addEdge "c" "d"
                , G.mergeEdge "b" "c" "e"
                ]
                [ assertEdgesVerts [("a", "e"), ("e", "d")] ["a", "e", "d"]
                , assertSuccs "a" ["e"]
                , assertSuccs "e" ["d"]
                , assertSuccs "d" []
                , assertPreds "a" []
                , assertPreds "e" ["a"]
                , assertPreds "d" ["e"]
                ]
            , graphTest "predecessor inheritance"
                -- The graph
                --     a - b
                --          \
                --           c
                --          /
                --         d
                -- should turn into
                --     a
                --      \
                --       e
                --      /
                --     d
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                , G.addEdge "d" "c"
                , G.mergeEdge "b" "c" "e"
                ]
                [ assertEdgesVerts [("a", "e"), ("d", "e")] ["a", "e", "d"]
                , assertPreds "e" ["a", "d"]
                , assertSuccs "a" ["e"]
                , assertSuccs "d" ["e"]
                ]
            , graphTest "other producer dependency"
                -- The graph
                --           e
                --          /
                --     a - b
                --          \
                --           c
                --          /
                --         d
                -- should turn into
                --       b - e
                --      /
                --     a
                --      \
                --       f
                --      /
                --     d
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                , G.addEdge "b" "e"
                , G.addEdge "d" "c"
                , G.mergeEdge "b" "c" "f"
                ]
                [ assertEdgesVerts [("a", "f"), ("d", "f"), ("a", "b"), ("b", "e")] ["a", "f", "d", "b", "e"]
                , assertPreds "f" ["a", "d"]
                , assertSuccs "a" ["f", "b"]
                , assertSuccs "b" ["e"]
                ]
            , graphTest "total graph deletion"
                -- the graph `a -> b` would be merged into `c`, but
                -- singular unconnected nodes shouldn't exist, so
                -- `c` should be deleted
                [ G.addEdge "a" "b"
                , G.mergeEdge "a" "b" "c"
                ]
                [ assertEdgesVerts [] []
                ]
            , graphTest "no producer predecessors"
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                , G.mergeEdge "a" "b" "d"
                ]
                [ assertEdgesVerts [("d", "c")] ["c", "d"]
                , assertPreds "d" []
                ]
            , graphTest "complex producer drop"
                --   e   f
                --    \ /
                -- a   d
                --  \ / \
                --   c   g
                --  /
                -- b
                [ G.addEdge "a" "c"
                , G.addEdge "b" "c"
                , G.addEdge "c" "d"
                , G.addEdge "e" "d"
                , G.addEdge "d" "f"
                , G.addEdge "d" "g"
                , G.mergeEdge "c" "d" "h"
                ]
                [ assertEdgesVerts [("a", "h"), ("b", "h"), ("e", "h"), ("h", "f"), ("h", "g")] ["a", "b", "h", "e", "f", "g"]
                , assertPreds "h" ["a", "b", "e"]
                , assertSuccs "h" ["f", "g"]
                ]
            , graphTest "complex no producer drop"
                --   e   f
                --    \ /
                -- a   d
                --  \ / \
                --   c   g
                --  / \ /
                -- b   i
                [ G.addEdge "a" "c"
                , G.addEdge "b" "c"
                , G.addEdge "c" "d"
                , G.addEdge "e" "d"
                , G.addEdge "d" "f"
                , G.addEdge "d" "g"
                , G.addEdge "c" "i"
                , G.addEdge "i" "g"
                , G.mergeEdge "c" "d" "h"
                ]
                [ assertEdgesVerts [("a", "h"), ("b", "h"), ("e", "h"), ("h", "f"), ("h", "g"), ("a", "c"), ("b", "c"), ("c", "i"), ("i", "g")] ["a", "b", "h", "e", "f", "g", "i", "c"]
                , assertPreds "h" ["a", "b", "e"]
                , assertSuccs "h" ["f", "g"]
                , assertPreds "c" ["a", "b"]
                , assertSuccs "c" ["i"]
                ]
            ]

        getSubgraphs = TestLabel "getSubgraphs" $ TestList
            [ TestLabel "sanity check" $ TestCase $
                case
                    sortWith (length . G.getVertices)
                    $ G.getSubgraphs
                    $ G.addEdge "a" "b"
                    $ G.addEdge "c" "d"
                    $ G.addEdge "d" "e"
                    G.empty
                    of
                    [sg1, sg2] -> do
                        assertEdgesVerts [("a", "b")] ["a", "b"] sg1
                        assertEdgesVerts [("c", "d"), ("d", "e")] ["c", "d", "e"] sg2
                    _ -> assertFailure "Did not get 2 subgraphs"
            , graphTest "sanity check 2"
                [ G.addEdge "a" "b"
                , G.addEdge "c" "d"
                , G.addEdge "d" "e"
                ]
                [ assertSubgraphVerts [["a", "b"], ["c", "d", "e"]]
                , assertSubgraphInvariants
                ]
            , graphTest "3 subgraphs"
                [ G.addEdge "a" "b"

                , G.addEdge "c" "d"
                , G.addEdge "d" "e"

                , G.addEdge "f" "g"
                , G.addEdge "g" "h"
                , G.addEdge "h" "i"
                ]
                [ assertSubgraphVerts [["a", "b"], ["c", "d", "e"], ["f", "g", "h", "i"]]
                , assertSubgraphInvariants
                ]
            , graphTest "deletion"
                [ G.addEdge "a" "b"
                , G.addEdge "b" "c"
                , G.addEdge "c" "d"
                , G.addEdge "d" "e"
                , G.removeEdge "b" "c"
                ]
                [ assertSubgraphVerts [["a", "b"], ["c", "d", "e"]]
                , assertSubgraphInvariants
                ]
            , graphTest "empty" [] [ assertSubgraphVerts [] ]
            , graphTest "no subgraphs"
                [ G.addEdge "a" "b"
                ]
                [ assertSubgraphVerts [["a", "b"]]
                , assertSubgraphInvariants
                ]
            ]

        getSuccessors = TestLabel "getSuccessors" $ TestList
            [ graphTest "absent vertex" [] [ assertSuccs "a" [] ]
            ]

        getPredecessors = TestLabel "getPredecessors" $ TestList
            [ graphTest "absent vertex" [] [ assertPreds "a" [] ]
            ]




