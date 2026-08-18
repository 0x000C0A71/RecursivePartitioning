{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Unique
    ( Unique()
    , newUnique
    , next
    , split
    , split2, split3, split4, split5
    ) where

import Numeric (showHex)


data Unique = Unique Int Int

newUnique :: Unique
newUnique = Unique 0 1

next :: Unique -> Unique
next (Unique val stride) = Unique (val + stride) stride

instance Eq Unique where
    (Unique v1 _) == (Unique v2 _) = v1 == v2
    (Unique v1 _) /= (Unique v2 _) = v1 /= v2

instance Ord Unique where
    compare (Unique v1 _) (Unique v2 _) = compare v1 v2

instance Show Unique where
    show (Unique v _) = showHex v ""


split :: Int -> Unique -> [Unique]
split n = take n . fmap mul_stride . inf
    where
        mul_stride :: Unique -> Unique
        mul_stride (Unique v s) = Unique v $ n * s

        inf :: Unique -> [Unique]
        inf u = k : inf k
            where
                k = next u


split2 :: Unique -> (Unique, Unique)
split2 = (\[u1, u2] -> (u1, u2)) . split 2

split3 :: Unique -> (Unique, Unique, Unique)
split3 = (\[u1, u2, u3] -> (u1, u2, u3)) . split 3

split4 :: Unique -> (Unique, Unique, Unique, Unique)
split4 = (\[u1, u2, u3, u4] -> (u1, u2, u3, u4)) . split 4

split5 :: Unique -> (Unique, Unique, Unique, Unique, Unique)
split5 = (\[u1, u2, u3, u4, u5] -> (u1, u2, u3, u4, u5)) . split 3




