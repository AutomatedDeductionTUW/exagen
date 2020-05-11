{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Propositional.Formula where

-- base
import Control.Monad (ap)

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- QuickCheck
import Test.QuickCheck



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

-- The size parameter (given through QuickCheck) is the number of connectives in the formula
genFormula'
  :: [Formula a -> Formula a]
  -> [Formula a -> Formula a -> Formula a]
  -> Gen a
  -> Gen (Formula a)
genFormula' unaryConnectives binaryConnectives genA = genF
  where
    genF = sized $ \sz -> do
      case sz of
        0 -> frequency freqBase
        n -> frequency (freqRecursive n)

    freqBase = [
      -- (1, pure Falsum),
      -- (1, pure Verum),
      (3, genAtomic)
      ]
    freqRecursive n = freqUnary n ++ freqBinary n
    freqUnary n = (\f -> (5, genUnary n f)) <$> unaryConnectives
    freqBinary n = (\f -> (5, genBinary n f)) <$> binaryConnectives

    genAtomic = Atomic <$> genA
    genUnary n f = resize (n - 1) (f <$> genF)
    genBinary n f = do
      let n' = n - 1  -- the binary connective
      leftSize <- chooseInt (0, n')  -- number of connectives in left argument
      let rightSize = n' - leftSize  -- number of connectives in right argument
      f <$> (resize leftSize genF) <*> (resize rightSize genF)

genFormula :: Gen a -> Gen (Formula a)
genFormula = genFormula' [Not] [And, Or, Imp, Iff]


-- TODO: there should be parentheses separating And/Or
prettyFormula
  :: forall a ann.
     (Int -> a -> Doc ann)
  -> Int
  -> Formula a
  -> Doc ann
prettyFormula prettyAtom = go
  where
    go :: Int -> Formula a -> Doc ann
    -- go _ Falsum = "False"
    -- go _ Verum = "True"
    go prec (Atomic x) = prettyAtom prec x
    go prec (Not p) = bracket (prec > 10) 1 (prettyPrefix 10) "~" p
    go prec (And p q) = bracket (prec > 8) 0 (prettyInfix 8 "/\\") p q
    go prec (Or p q)  = bracket (prec > 6) 0 (prettyInfix 6 "\\/") p q
    go prec (Imp p q) = bracket (prec > 4) 0 (prettyInfix 4 "==>") p q
    go prec (Iff p q) = bracket (prec > 2) 0 (prettyInfix 2 "<=>") p q
    prettyPrefix :: Int -> Doc ann -> Formula a -> Doc ann
    prettyPrefix newPrec sym p = sym <> go (newPrec+1) p  -- TODO: Remove +1 here to save parens on nested negations
    prettyInfix :: Int -> Doc ann -> Formula a -> Formula a -> Doc ann
    prettyInfix newPrec sym p q = go (newPrec+1) p <+> sym <> line <> go newPrec q
    bracket :: forall b c. Bool -> Int -> (b -> c -> Doc ann) -> b -> c -> Doc ann
    bracket br n f x y = (if br then parens else id) (nest n (align $ group $ f x y))

-- instance Pretty a => Show (Formula a) where
--   showsPrec _ = renderShowS . layoutPretty layoutOptions . enclose "[p| " " |]" . align . pretty
--     where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

instance Pretty a => Pretty (Formula a) where
  pretty = prettyFormula (const pretty) 0


-- | Primitive propositions
newtype Prop = P { pName :: String }
  deriving (Eq, Ord)

instance Show Prop where
  show = pName

instance Pretty Prop where
  pretty = pretty . pName

instance Arbitrary Prop where
  arbitrary = P . getIdentifier <$> arbitrary



-- Helper type to generate valid identifiers with QuickCheck
newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Eq, Ord)

instance Arbitrary Identifier where
  arbitrary = genIdentifier

genIdentifier :: Gen Identifier
genIdentifier = Identifier <$> ((:) <$> genFirstChar <*> listOf genNextChar)
  where
    genFirstChar = elements $ ['a'..'z'] <> ['_']
    genNextChar = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['\'', '_'] <> ['0'..'9']







-- flatten before printing?
data FlatFormula a
  = FlatAtomic a
  | FlatNot (Formula a)
  | FlatAnd [Formula a]
  | FlatOr [Formula a]
  | FlatIff (Formula a)
  | FlatImp (Formula a)
  deriving (Show, Eq, Ord)
