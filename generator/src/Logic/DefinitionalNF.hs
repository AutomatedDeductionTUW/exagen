{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Logic.DefinitionalNF
  ( Polarity(..)
  , flipPolarity
  , polarity
  , atomPolarity
  , hasAtomPolarity
  , Stream(..)
  , definitionalNF
  ) where

-- base
import Data.Foldable
import Data.List

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

-- deriving-compat
-- import Text.Show.Deriving

-- mtl
import Control.Monad.State.Strict

-- recursion-schemes
-- import Data.Functor.Foldable (Fix(..))

-- exagen
import Logic.Formula



data Polarity
  = Pos
  | Neg
  | Both
  deriving (Eq, Show)

instance Semigroup Polarity where
  Pos <> Pos = Pos
  Neg <> Neg = Neg
  _ <> _ = Both

flipPolarity :: Polarity -> Polarity
flipPolarity Pos = Neg
flipPolarity Neg = Pos
flipPolarity Both = Both

-- | Annotate each atom occurrence with its polarity
polarity :: Formula a -> Formula (a, Polarity)
polarity = go Pos
  where
    go pol (Atomic x) = Atomic (x, pol)
    go _   (Const x) = Const x
    go pol (Not f) = Not (go (flipPolarity pol) f)
    go pol (And f g) = And (go pol f) (go pol g)
    go pol (Or  f g) = Or  (go pol f) (go pol g)
    go pol (Imp f g) = Imp (go (flipPolarity pol) f) (go pol g)
    go _   (Iff f g) = Iff (go Both f) (go Both g)

atomPolarity :: Ord a => Formula a -> Map a Polarity
atomPolarity = Map.fromListWith (<>) . toList . polarity

hasAtomPolarity :: Ord a => Polarity -> Formula a -> Bool
hasAtomPolarity pol = (pol `elem`) . map snd . Map.toList . atomPolarity


{-
data Ann ann f a = Ann !ann (f a)
  deriving (Eq, Ord, Show, Functor)

-- instance Show1 f => Show1 (Ann ann f) where
$(deriveShow1 ''Ann)


-- | Annotate each subformula with its polarity
polarity' :: Formula a -> Fix (Ann Polarity (FormulaF a))
polarity' = go Pos
  where
    go pol f = Fix $ Ann pol (go' pol f)

    go' _   (Atomic x) = AtomicF x
    go' _   (Const x)  = ConstF x
    go' pol (Not f)    = NotF (go (flipPolarity pol) f)
    go' pol (And f g)  = AndF (go pol f) (go pol g)
    go' pol (Or  f g)  = OrF  (go pol f) (go pol g)
    go' pol (Imp f g)  = ImpF (go (flipPolarity pol) f) (go pol g)
    go' _   (Iff f g)  = IffF (go Both f) (go Both g)


annotations :: Foldable f => Fix (Ann ann f) -> [ann]
annotations (Fix (Ann x f)) = [x] ++ foldMap annotations f
-}



-- | An infinite stream
data Stream a = !a ::: Stream a
  deriving Functor

data St a = St
  { newNames :: !(Stream a)
  , definitions :: !(Map (Formula a) (a, Polarity))
  }

-- | Returns name of top-level formula and a list of definitions for each subformula.
definitionalNF :: forall a. Ord a => Stream a -> Formula a -> (a, [Formula a])
definitionalNF initialNewNames = \fm ->
  let (fmName, finalState) = runState (transform fm Pos) initialState
  in (fmName, extractResult finalState)

  where
    initialState = St{ newNames = initialNewNames
                     , definitions = mempty
                     }

    buildDefinition :: Formula a -> (a, Polarity) -> Formula a
    buildDefinition fm (n, Pos)  = Imp (Atomic n) fm
    buildDefinition fm (n, Neg)  = Imp fm (Atomic n)
    buildDefinition fm (n, Both) = Iff (Atomic n) fm

    -- The formulas returned by extractResult are pairwise distinct
    extractResult =
      fmap (uncurry buildDefinition)
      . sortOn (\(_,(n,_)) -> n)
      . Map.toList
      . definitions

    -- | Return the name for a given subformula, or create a new one
    getName :: Formula a -> Polarity -> State (St a) a
    getName fm pol = do
      defs <- gets definitions
      case Map.lookup fm defs of
        Nothing -> do
          (n ::: ns) <- gets newNames
          put St{ newNames = ns
                , definitions = Map.insert fm (n, pol) defs
                }
          return n
        Just (n, p) -> do
          let p' = p <> pol
          when (p /= p') $
            modify' $ \st -> st{ definitions = Map.insert fm (n, p') defs }
          return n

    transform :: Formula a  -- ^ the current subformula being transformed
              -> Polarity   -- ^ the polarity of the current subformula
              -> State (St a) a
    transform f@(Const _) = transform0 f
    transform (Atomic n) = const (return n)
    transform (Not f) = transform1 Not (f, decreasing)
    transform (And f g) = transform2 And (f, increasing)   (g, increasing)
    transform (Or  f g) = transform2 Or  (f, increasing)   (g, increasing)
    transform (Imp f g) = transform2 Imp (f, decreasing)   (g, increasing)
    transform (Iff f g) = transform2 Iff (f, nonmonotonic) (g, nonmonotonic)

    transform0 :: Formula a -> Polarity -> State (St a) a
    transform0 fm pol = do
      n <- getName fm pol
      return n

    transform1 :: (Formula a -> Formula a)
               -> (Formula a, Monotonicity)
               -> Polarity
               -> State (St a) a
    transform1 op (f, mon_f) pol = do
      nf <- transform f (mon_f pol)
      let fm = op (Atomic nf)
      n <- getName fm pol
      return n

    transform2 :: (Formula a -> Formula a -> Formula a)
               -> (Formula a, Monotonicity)
               -> (Formula a, Monotonicity)
               -> Polarity
               -> State (St a) a
    transform2 op (f, mon_f) (g, mon_g) pol = do
      ng <- transform g (mon_g pol)
      nf <- transform f (mon_f pol)
      let fm = op (Atomic nf) (Atomic ng)
      n <- getName fm pol
      return n


type Monotonicity = Polarity -> Polarity

increasing :: Monotonicity
increasing = id

decreasing :: Monotonicity
decreasing = flipPolarity

nonmonotonic :: Monotonicity
nonmonotonic = const Both
