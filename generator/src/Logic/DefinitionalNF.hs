{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- mtl
import Control.Monad.State.Strict

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

-- Annotate each atom occurrence with its polarity
-- TODO: if we define Formula as higher-kinded data type we can probably use recursion-schemes to annotate each subformula with polarity, not just the atoms.
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


{-  old version below

-- data Lit a = LPos a | LNeg a
-- newtype Clause a = Clause [Lit a]
-- newtype CNF a = CNF [Clause a]

data St a = St
  { stNewNames :: !(Stream a)
  , stNames :: !(HashMap (Formula Void a) (a, OccursWithPolarity))
  }

data Stream a = !a ::: Stream a

-- | The polarity with which a certain subformula occurs in the whole formula.
data OccursWithPolarity = Pos | Neg | Both
  deriving Eq

instance Semigroup OccursWithPolarity where
  Pos <> Pos = Pos
  Neg <> Neg = Neg
  _ <> _ = Both

type Monotonicity = OccursWithPolarity -> OccursWithPolarity
monIncr :: Monotonicity
monIncr = id
monDecr :: Monotonicity
monDecr Pos = Neg
monDecr Neg = Pos
monDecr Both = Both
monNone :: Monotonicity
monNone = const Both

-- definitionalNF :: Stream a -> Formula Void a -> CNF a
-- definitionalNF newNames = error "todo"
--   where
--     mkName :: State (St a) a
--     -- go :: State St (CNF a)

streamOfNewNames :: Formula Void Prop -> Stream Prop
streamOfNewNames fm = go 1
  where go :: Integer -> Stream Prop
        go i = (P (n ++ show i)) ::: go (i + 1)
        n = newNamePrefix fm

-- We need to find some name that isn't a prefix of any name occuring in the formula.
newNamePrefix :: Formula Void Prop -> String
newNamePrefix fm
  | null fm = "_n"
  | getAll $ foldMap (All . not . ("_n" `isPrefixOf`) . pName) fm = "_n"
  | otherwise = pName (maximum fm) ++ "_n"

definitionalNF'' :: Formula Void Prop -> (Prop, [Formula Void Prop])
definitionalNF'' fm = definitionalNF' (streamOfNewNames fm) fm

definitionalNF' :: forall a. (Eq a, Hashable a) => Stream a -> Formula Void a -> (a, [Formula Void a])
definitionalNF' newNames = \fm -> extractResult <$> runState (transform fm Pos) initialState
  where
    initialState = St { stNewNames = newNames
                      , stNames = mempty
                      }
    buildDefinition :: Formula Void a -> (a, OccursWithPolarity) -> Formula Void a
    buildDefinition fm (n, Pos)  = Imp (Atomic n) fm
    buildDefinition fm (n, Neg)  = Imp fm (Atomic n)
    buildDefinition fm (n, Both) = Iff (Atomic n) fm
    -- the formulas returned by extractResult are pairwise distinct
    extractResult = fmap (uncurry buildDefinition) . HashMap.toList . stNames
    -- | Return the name for a given subformula, or create a new one
    mkName :: Formula Void a -> OccursWithPolarity -> State (St a) a
    mkName fm pol = do
      names <- gets stNames
      case HashMap.lookup fm names of
        Nothing -> do (n ::: ns) <- gets stNewNames
                      put St { stNewNames = ns
                             , stNames = HashMap.insert fm (n, pol) names }
                      return n
        Just (n, p) -> do let newPol = p <> pol
                          when (p /= newPol) $ do
                            modify' $ \st -> st { stNames = HashMap.insert fm (n, newPol) names }
                          return n
    transform :: Formula Void a  -- ^ the current subformula being transformed
              -> OccursWithPolarity  -- ^ the polarity of the current subformula
              -> State (St a) a
    transform Verum = tfConst Verum
    transform Falsum = tfConst Falsum
    transform (Atomic n) = const $ return n --, mempty)
    transform (Not subFm) = tfUnary Not (subFm, monDecr)
    transform (And subFm1 subFm2) = tfBinary And (subFm1, monIncr) (subFm2, monIncr)
    transform (Or  subFm1 subFm2) = tfBinary Or  (subFm1, monIncr) (subFm2, monIncr)
    transform (Imp subFm1 subFm2) = tfBinary Imp (subFm1, monDecr) (subFm2, monIncr)
    transform (Iff subFm1 subFm2) = tfBinary Iff (subFm1, monNone) (subFm2, monNone)
    transform (Exists v _) = absurd v
    transform (Forall v _) = absurd v
    tfConst :: Formula Void a
            -> OccursWithPolarity
            -> State (St a) a
    tfConst fm pol = do
      n <- mkName fm pol
      return n
    tfUnary :: (Formula Void a -> Formula Void a)
            -> (Formula Void a, Monotonicity)
            -> OccursWithPolarity
            -> State (St a) a
    tfUnary op (subFm, mon) pol = do
      subN <- transform subFm (mon pol)
      let fm = op (Atomic subN)
      n <- mkName fm pol
      return n
    tfBinary :: (Formula Void a -> Formula Void a -> Formula Void a)
             -> (Formula Void a, Monotonicity)
             -> (Formula Void a, Monotonicity)
             -> OccursWithPolarity
             -> State (St a) a
    tfBinary op (subFm1, mon1) (subFm2, mon2) pol = do
      subN1 <- transform subFm1 (mon1 pol)
      subN2 <- transform subFm2 (mon2 pol)
      let fm = op (Atomic subN1) (Atomic subN2)
      n <- mkName fm pol
      return n
-}
