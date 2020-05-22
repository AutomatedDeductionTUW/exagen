{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Monad.Choose where

-- base
import Control.Applicative
import Data.Functor.Identity

-- combinat
import Math.Combinat.Permutations

-- list-transformer
import List.Transformer

-- mtl
import Control.Monad.Reader
import Control.Monad.State.Strict

-- random
import System.Random hiding (next)



class MonadPlus m => MonadChoose m where
  choose :: [a] -> m a

-- NOTE: don't use this instance, the built-in list monad seems to build the full list in memory before returning
-- instance MonadChoose [] where
--   {-# INLINE choose #-}
--   choose = id

instance MonadChoose m => MonadChoose (StateT s m) where
  {-# INLINE choose #-}
  choose = lift . choose

instance MonadChoose m => MonadChoose (ReaderT r m) where
  {-# INLINE choose #-}
  choose = lift . choose

instance Monad m => MonadChoose (ListT m) where
  {-# INLINE choose #-}
  choose = select


-- | Random choice with support for proper backtracking.
newtype RandomListT g m a = RandomListT { unRandomListT :: ListT (StateT g m) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadTrans (RandomListT g) where
  {-# INLINE lift #-}
  lift = RandomListT . lift . lift

instance (Monad m, RandomGen g) => MonadChoose (RandomListT g m) where
  {-# INLINE choose #-}
  choose [] =
    mzero
  choose [x] =
    return x
  choose xs = do
    let n = length xs
    p <- withRandomGen (randomPermutation n)
    let xs' = permuteList p xs
    select xs'

{-# INLINE withRandomGen #-}
withRandomGen :: Monad m => (g -> (a, g)) -> RandomListT g m a
withRandomGen = RandomListT . lift . state

runRandomListT :: Monad m => RandomListT g m a -> g -> m (Maybe a, g)
runRandomListT r g =
  flip runStateT g $ do
    s <- next (unRandomListT r)
    case s of
      Nil -> return Nothing
      Cons x _ -> return (Just x)

evalRandomListIO :: Monad m => (forall x. m x -> IO x) -> RandomListT StdGen m a -> IO (Maybe a)
evalRandomListIO unlift r = do
  g <- getStdGen
  (x, g') <- unlift (runRandomListT r g)
  setStdGen g'
  return x

evalRandomListIO' :: RandomListT StdGen Identity a -> IO (Maybe a)
evalRandomListIO' = evalRandomListIO (pure . runIdentity)





blah :: MonadChoose m => StateT Int m String
blah = do
  k <- get
  x <- choose [k .. k + 5]
  return (show x)

-- allBlah :: Int -> [String]
-- allBlah = evalStateT (blah @[])

allBlah' :: Int -> [String]
allBlah' = runIdentity . List.Transformer.fold (flip (:)) [] reverse . evalStateT (blah @(ListT Identity))

randomBlah :: Int -> IO (Maybe String)
randomBlah = evalRandomListIO' . evalStateT (blah @(RandomListT StdGen Identity))
