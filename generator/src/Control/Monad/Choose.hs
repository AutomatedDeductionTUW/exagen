{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Monad.Choose where

-- base
import Control.Applicative
import Control.Monad.Fail
import Data.Functor.Identity

-- combinat
import Math.Combinat.Permutations

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- list-transformer
import List.Transformer as ListT

-- mtl
import Control.Monad.Reader
import Control.Monad.State.Strict

-- random
import System.Random hiding (next)



randomPermutationWeighted :: forall a g. RandomGen g => [(Int, a)] -> g -> ([a], g)
randomPermutationWeighted [] = \g -> ([], g)
randomPermutationWeighted [(_,x)] = \g -> ([x], g)
randomPermutationWeighted choices = go totalWeight choices
  where
    totalWeight = sum (map fst choices)

    go :: Int -> [(Int, a)] -> g -> ([a], g)
    go 0 [] g = ([], g)
    go total wxs g =
      let (randomIndex, g') = randomR (1, total) g
      in case splitAtWeight randomIndex fst wxs of
           (_, []) -> error "bug"
           (wxs', (w,x):wxs'') ->
             let (xs, g'') = go (total - w) (wxs' ++ wxs'') g'
             in (x:xs, g'')


-- | Splits off as many elements from the given list as possible
-- while keeping the sum of their weights smaller than the given integer.
--
-- More precisely: 'splitAtWeight n w xs = (ys, zs)' where 'xs = ys + zs'
-- and 'ys' is the longest prefix of 'xs' such that 'sum (map w ys) < n'.
splitAtWeight :: Int -> (a -> Int) -> [a] -> ([a], [a])
splitAtWeight _ _ [] = ([], [])
splitAtWeight n w (x:xs)
  | n <= wx = ([], x:xs)
  | otherwise =
      let (xs', xs'') = splitAtWeight (n - wx) w xs
      in (x:xs', xs'')
  where
    wx = w x
{-# INLINE splitAtWeight #-}


-- | Monad with an operation to choose an element from a list.
--
-- The goal is to support the following use cases:
-- * random choice
-- * select n distinct random choices
-- * enumeration of the sample space
class MonadPlus m => MonadChoose m where
  -- | Choose an element with uniform probability
  choose :: [a] -> m a

  -- | Choose an element with weighted probability
  chooseWeighted :: [(Int, a)] -> m a


-- chooseAny :: (Bounded a, Enum a, MonadChoose m) => m a
-- chooseAny = choose [minBound .. maxBound]


instance MonadChoose [] where
  {-# INLINE choose #-}
  choose = id
  {-# INLINE chooseWeighted #-}
  chooseWeighted = map snd

instance MonadChoose m => MonadChoose (StateT s m) where
  {-# INLINE choose #-}
  choose = lift . choose
  {-# INLINE chooseWeighted #-}
  chooseWeighted = lift . chooseWeighted

instance MonadChoose m => MonadChoose (ReaderT r m) where
  {-# INLINE choose #-}
  choose = lift . choose
  {-# INLINE chooseWeighted #-}
  chooseWeighted = lift . chooseWeighted

instance Monad m => MonadChoose (ListT m) where
  {-# INLINE choose #-}
  choose = select
  {-# INLINE chooseWeighted #-}
  chooseWeighted = choose . map snd


-- | Random choice with support for proper backtracking.
--
-- Note that randomness is only introduced by the 'MonadChoose' instance.
-- 'Alternative' and 'MonadPlus' perform deterministic branching.
newtype RandomListT g m a = RandomListT { unRandomListT :: ListT (StateT g m) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail)

{-
instance (Monad m, RandomGen g) => Alternative (RandomListT g m) where
  {-# INLINE empty #-}
  empty = RandomListT empty

  {-# INLINE (<|>) #-}
  r1 <|> r2 = do
    -- Choose randomly from which side we take the first element
    b <- withRandomGen random
    let (RandomListT l, r) = if b then (r1, r2) else (r2, r1)

    s <- RandomListT $ lift $ next l
    case s of
      Nil -> r
      Cons x l' ->
        let r' = RandomListT l' <|> r
        in RandomListT $ ListT $ return $ Cons x (unRandomListT r')

instance (Monad m, RandomGen g) => MonadPlus (RandomListT g m) where
  {-# INLINE mzero #-}
  mzero = empty
  {-# INLINE mplus #-}
  mplus = (<|>)
-}

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
    RandomListT $ select @_ @(ListT _) xs'

  {-# INLINE chooseWeighted #-}
  chooseWeighted [] = mzero
  chooseWeighted [(_,x)] = return x
  chooseWeighted wxs = do
    xs' <- withRandomGen (randomPermutationWeighted wxs)
    RandomListT $ select @_ @(ListT _) xs'


{-# INLINE withRandomGen #-}
withRandomGen :: Monad m => (g -> (a, g)) -> RandomListT g m a
withRandomGen = RandomListT . lift . state


evalRandomListT :: Monad m => RandomListT g m a -> g -> m (Maybe a, g)
evalRandomListT r g =
  flip runStateT g $ do
    s <- next (unRandomListT r)
    case s of
      Nil -> return Nothing
      Cons x _ -> return (Just x)

evalRandomListIO :: Monad m => (forall x. m x -> IO x) -> RandomListT StdGen m a -> IO (Maybe a)
evalRandomListIO unlift r = do
  g <- getStdGen
  (x, g') <- unlift (evalRandomListT r g)
  setStdGen g'
  return x


{-
-- | Note: this gives a bad distribution of random values.
-- Since backtracking still happens chronologically, the
-- stream will first contain all variations of the last 'choose'
-- until getting to the first change in second-to-last 'choose', and so on.
streamRandomListT :: Monad m => RandomListT g m a -> g -> ListT m a
streamRandomListT (RandomListT r) g = ListT $ do
  (s, g') <- runStateT (next r) g
  case s of
    Nil -> return Nil
    Cons x s' -> let r' = streamRandomListT (RandomListT s') g'
                 in return (Cons x r')

-- streamRandomListIO' :: Monad m => (forall x. m x -> IO x) -> RandomListT StdGen m a -> ListT IO a
-- streamRandomListIO' unlift r = ListT $ do
--   g <- newStdGen
--   let l1 = streamRandomListT r g
--   next (ListT.unfold (\l -> do s <- unlift (next l)
--                                case s of
--                                  Nil -> return Nothing
--                                  Cons x l' -> return (Just (x, l'))) l1)

-- | Note: this gives a bad distribution of random values.
-- Since backtracking still happens chronologically, the
-- stream will first contain all variations of the last 'choose'
-- until getting to the first change in second-to-last 'choose', and so on.
streamRandomListIO :: RandomListT StdGen IO a -> ListT IO a
streamRandomListIO (RandomListT r) = ListT $ do
  g <- getStdGen
  (s, g') <- runStateT (next r) g
  setStdGen g'
  case s of
    Nil -> return Nil
    Cons x s' -> let r' = streamRandomListIO (RandomListT s')
                 in return (Cons x r')
-}


type RandomChoice = RandomListT StdGen Identity

evalRandomChoice :: RandomChoice a -> StdGen -> (Maybe a, StdGen)
evalRandomChoice r g = runIdentity $ evalRandomListT r g

evalRandomChoiceIO :: RandomChoice a -> IO (Maybe a)
evalRandomChoiceIO r = getStdRandom (evalRandomChoice r)


distinctRandomChoices' :: forall a b. Ord b => Int -> (a -> b) -> RandomChoice a -> IO (Either String [a])
distinctRandomChoices' n normalize r = go n Set.empty
  where
    go :: Int -> Set b -> IO (Either String [a])
    go 0 _   = return (Right [])
    go k seen = do
      -- Generate an element whose normalized form hasn't been seen yet
      mx <-
        evalRandomChoiceIO $
        mfilter ((`Set.notMember` seen) . normalize) r
      case mx of
        Nothing ->
          return (Left ("empty sample space after generating " <> show (n - k) <> " samples"))
        Just x -> do
          rest <- go (k - 1) (Set.insert (normalize x) seen)
          return (fmap (x:) rest)

distinctRandomChoices :: Ord a => Int -> RandomChoice a -> IO (Either String [a])
distinctRandomChoices n = distinctRandomChoices' n id

distinctRandomChoicesFail :: Ord a => Int -> RandomChoice a -> IO [a]
distinctRandomChoicesFail n r =
  distinctRandomChoices' n id r
  >>= either Control.Monad.Fail.fail pure





blah :: MonadChoose m => StateT Int m String
blah = do
  k <- get
  x <- choose [k .. k + 5]
  y :: [Int] <- replicateM 3 (choose [1 .. 3])
  -- x <- chooseWeighted [(75, k), (5, k+1), (5, k+2), (5, k+3), (5, k+4), (5, k+5)]
  put (x+10)  -- Note that the state follows along each branch, and backtracking also resets the state
  return (show (x, y))

blahAlt :: MonadChoose m => StateT Int m String
blahAlt = do
  k <- get
  (put (100*k) >> blah) <|> (put (10000*k) >> blah)

-- | List all values
allBlah :: Int -> [String]
allBlah = evalStateT (blah @[])

-- | Streams all values
allBlahStream :: forall m. Monad m => Int -> ListT m String
allBlahStream = evalStateT (blah @(ListT m))

-- randomBlahStream :: Int -> ListT IO String
-- randomBlahStream = streamRandomListIO . evalStateT blah


-- Usage:
--
-- To print 3 random values:
--     printStream (ListT.take 3 (randomBlahStream 27))
--
-- To print all values:
--     printStream (allBlahStream 27)


printStream :: Show a => ListT IO a -> IO ()
printStream stream = runListT $ do x <- stream
                                   liftIO $ print x

randomBlah :: Int -> IO (Maybe String)
randomBlah = evalRandomChoiceIO . evalStateT (blah @RandomChoice)
