
module Types
    ( Fusion
    , NoFusion
    , FuseNoFuses
    , MonadPar(..)
    , Budget
    , Quality
    , Reg(..)
    , CounterM()
    , runCounterM
    , add, inc
    ) where

import Control.Parallel.Strategies (evalTuple2, rseq, using, parTuple2, rdeepseq)
import Control.Concurrent.Async    (withAsync, wait, mapConcurrently)

import qualified Data.Set                    as S
import qualified Control.Parallel.Strategies as PS


data Reg
    = OrigReg String
    | RenameReg String
    deriving (Show, Eq, Ord)

type Budget = Double
type Quality = Float


type Fusion v = (v, v, v)
type NoFusion v = (v, v)
type FuseNoFuses v = ([Fusion v], S.Set (NoFusion v))


class Monad m => MonadPar m where
    par2 :: (m a, m b) -> m (a, b)
    parList :: [m a] -> m [a]

    {-# INLINE par3 #-}
    par3 :: (m a, m b, m c) -> m (a, b, c)
    par3 (a, b, c) = flip fmap (par2 (par2 (a, b), c)) $ \((a', b'), c') -> (a', b', c')

    {-# INLINE par4 #-}
    par4 :: (m a, m b, m c, m d) -> m (a, b, c, d)
    par4 (a, b, c, d) = flip fmap (par2 (par2 (a, b), par2 (c, d))) $ \((a', b'), (c', d')) -> (a', b', c', d')

    {-# INLINE par5 #-}
    par5 :: (m a, m b, m c, m d, m e) -> m (a, b, c, d, e)
    par5 (a, b, c, d, e) = flip fmap (par2 (par4 (a, b, c, d), e)) $ \((a', b', c', d'), e') -> (a', b', c', d', e')


instance MonadPar IO where
    {-# INLINE par2 #-}
    {-# INLINE par3 #-}
    {-# INLINE parList #-}
    par2 (act1, act2) = withAsync act2 $ \thread -> do
        res1 <- act1
        res2 <- wait thread
        return (res1, res2)

    par3 (act1, act2, act3) = withAsync act2 $ \thread2 -> withAsync act3 $ \thread3 -> do
        res1 <- act1
        res2 <- wait thread2
        res3 <- wait thread3
        return (res1, res2, res3)

    parList = mapConcurrently id


data CounterM a = CounterM !a !Int

runCounterM :: CounterM a -> (a, Int)
runCounterM (CounterM a n) = (a, n)

instance Functor CounterM where
    {-# INLINE fmap #-}
    fmap f (CounterM a n) = CounterM (f a) n

instance Applicative CounterM where
    {-# INLINE liftA2 #-}
    {-# INLINE (<*>) #-}
    {-# INLINE pure #-}

    liftA2 f (CounterM a n1) (CounterM b n2) = CounterM (f a b) (n1 + n2)
    (CounterM f n1) <*> (CounterM a n2) = CounterM (f a) (n1 + n2)
    pure a = CounterM a 0

instance Monad CounterM where
    {-# INLINE (>>=) #-}
    (CounterM a n1) >>= f = CounterM b (n1 + n2)
        where
            (CounterM b n2) = f a

instance MonadPar CounterM where
    {-# INLINE par2 #-}
    {-# INLINE parList #-}
    par2 (act1, act2) = CounterM (a, b) (w1 + w2)
        where
            ((a, w1), (b, w2)) = (runCounterM act1, runCounterM act2) `using` parTuple2 seq_tup seq_tup
            seq_tup = evalTuple2 rseq rdeepseq

    parList acts = CounterM els (sum counts)
        where
            parred = fmap runCounterM acts `using` PS.parList seq_tup
            (els, counts) = unzip parred
            seq_tup = evalTuple2 rseq rdeepseq

add :: Int -> CounterM ()
add = CounterM ()

inc :: CounterM ()
inc = add 1
