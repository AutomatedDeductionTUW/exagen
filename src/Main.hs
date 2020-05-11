{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- base
import Control.Monad (ap)

-- QuickCheck
import Test.QuickCheck


main :: IO ()
main = do
  putStrLn "hello world"
  sample (arbitrary @(Formula Int))


data Formula a
  = Atomic a
  -- | Falsum
  -- | Verum
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or  (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative Formula where
  pure :: a -> Formula a
  pure = Atomic
  (<*>) :: Formula (a -> b) -> Formula a -> Formula b
  (<*>) = ap

-- Monadic bind for formulas is substitution of atoms
instance Monad Formula where
  return :: a -> Formula a
  return = pure
  (>>=) :: Formula a -> (a -> Formula b) -> Formula b
  fm >>= f = joinFormula (f <$> fm)

joinFormula :: Formula (Formula a) -> Formula a
-- joinFormula Falsum = Falsum
-- joinFormula Verum = Verum
joinFormula (Atomic fm) = fm
joinFormula (Not fm) = Not (joinFormula fm)
joinFormula (And fm1 fm2) = And (joinFormula fm1) (joinFormula fm2)
joinFormula (Or  fm1 fm2) = Or  (joinFormula fm1) (joinFormula fm2)
joinFormula (Imp fm1 fm2) = Imp (joinFormula fm1) (joinFormula fm2)
joinFormula (Iff fm1 fm2) = Iff (joinFormula fm1) (joinFormula fm2)
-- joinFormula (Forall s fm) = Forall s (joinFormula fm)
-- joinFormula (Exists s fm) = Exists s (joinFormula fm)


instance Arbitrary a => Arbitrary (Formula a) where
  arbitrary = genFormula arbitrary

genFormula'
  :: [Formula a -> Formula a]
  -> [Formula a -> Formula a -> Formula a]
  -> Gen a
  -> Gen (Formula a)
genFormula' unaryConnectives binaryConnectives genA = genF
  where
    genF = sized $ \sz ->
      case sz of
        0 -> frequency freqBase
        n -> frequency (freqRecursive n)

    freqBase = [
      -- (1, pure Falsum),
      -- (1, pure Verum),
      (3, genAtomic)
      ]
    freqRecursive n = freqBase ++ freqUnary n ++ freqBinary n
    freqUnary n = (\f -> (5, genUnary n f)) <$> unaryConnectives
    freqBinary n = (\f -> (5, genBinary n f)) <$> binaryConnectives

    genAtomic = Atomic <$> genA
    genUnary n f = resize (n - 1) (f <$> genF)
    genBinary n f = resize (n `div` 2) (f <$> genF <*> genF)  -- TODO: randomize L/R balance? => no this comes in by choosing 'Atomic' early in the recursive case

genFormula :: Gen a -> Gen (Formula a)
genFormula = genFormula' [Not] [And, Or, Imp, Iff]










-- flatten before printing?
data FlatFormula a
  = FlatAtomic a
  | FlatNot (Formula a)
  | FlatAnd [Formula a]
  | FlatOr [Formula a]
  | FlatIff (Formula a)
  | FlatImp (Formula a)
  deriving (Show, Eq, Ord)
