{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Formula where

-- base
import Control.Monad (ap)
import Data.Foldable
import Data.List (nub, sort)
import Data.Maybe (fromJust)

-- containers
-- import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- QuickCheck
import Test.QuickCheck


data Formula a
  = Atomic !a
  | Const !Bool
  | Not (Formula a)
  | Binary !Conn2 (Formula a) (Formula a)
  deriving (Eq, Ord, Show, Functor, Foldable)
{-# COMPLETE Atomic, Const, Not, And, Or, Iff, Imp :: Formula #-}

data Conn2 = And' | Or' | Iff' | Imp'
  deriving (Eq, Ord, Show)

pattern And :: Formula a -> Formula a -> Formula a
pattern And f g = Binary And' f g

pattern Or :: Formula a -> Formula a -> Formula a
pattern Or f g = Binary Or' f g

pattern Iff :: Formula a -> Formula a -> Formula a
pattern Iff f g = Binary Iff' f g

pattern Imp :: Formula a -> Formula a -> Formula a
pattern Imp f g = Binary Imp' f g

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
joinFormula (Atomic fm) = fm
joinFormula (Const b) = Const b
joinFormula (Not fm) = Not (joinFormula fm)
joinFormula (Binary conn fm1 fm2) = Binary conn (joinFormula fm1) (joinFormula fm2)

instance Arbitrary a => Arbitrary (Formula a) where
  arbitrary = genFormula arbitrary
  shrink (Atomic _) = []
  shrink (Const _) = []
  shrink (Not f) = [f] ++ (Not <$> shrink f)
  shrink (Binary c f g) = [f, g] ++ (Binary c <$> shrink f <*> pure g) ++ (Binary c f <$> shrink g)

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
      let n' = n - 1  -- the binary connective takes one off the size
      leftSize <- chooseInt (0, n')  -- number of connectives in left argument
      let rightSize = n' - leftSize  -- number of connectives in right argument
      f <$> (resize leftSize genF) <*> (resize rightSize genF)

genFormula :: Gen a -> Gen (Formula a)
genFormula = genFormula' [Not] [And, Or, Imp, Iff]


eval :: Formula Bool -> Bool
eval (Atomic b) = b
eval (Const b) = b
eval (Not f) = not (eval f)
eval (And f g) = eval f && eval g
eval (Or f g) = eval f || eval g
eval (Iff f g) = eval f == eval g
eval (Imp f g) = not (eval f) || eval g


allAssignments :: [a] -> [[(a, Bool)]]
allAssignments [] = [[]]
allAssignments (x:xs) =
  [ (x, v) : assignment | assignment <- allAssignments xs, v <- [ True, False ] ]


-- | Naive satisfiability test (evaluates the formula under all interpretations)
satisfiable :: Eq a => Formula a -> Bool
satisfiable fm = or [ evalUnderAssignment a fm | a <- allAssignments (distinctAtoms fm) ]
  where
    distinctAtoms = nub . toList
    evalUnderAssignment a = eval . fmap (\x -> fromJust (lookup x a))


-- | Naive validity test (evaluates the formula under all interpretations)
valid :: Eq a => Formula a -> Bool
valid = not . satisfiable . Not


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
    go _ (Const False) = "False"
    go _ (Const True) = "True"
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





-- | Normalizes a formula to remove certain superficial differences
--
-- * Associativity of nested And/Or
-- * Commutativity of And/Or/Iff
-- * Finally, all atoms are uniformly replaced by integers
normalize :: Ord a => Formula a -> FlatFormula Int
normalize = sortFlatFormula . normalizeAtoms . sortFlatFormula . flatten
-- normalize = normalizeAtomsIso . sortFlatFormula . flatten

-- | Flattens associative binary connectives (And/Or)
flatten :: Formula a -> FlatFormula a
flatten (Atomic a) = FlatAtomic a
flatten (Const b) = FlatConst b
flatten (Not f) = FlatNot (flatten f)
flatten (Iff f g) = FlatIff (flatten f) (flatten g)
flatten (Imp f g) = FlatImp (flatten f) (flatten g)
flatten (And f g) = FlatAnd (conjuncts (flatten f) ++ conjuncts (flatten g))
flatten (Or  f g) = FlatOr  (disjuncts (flatten f) ++ disjuncts (flatten g))

conjuncts :: FlatFormula a -> [FlatFormula a]
conjuncts (FlatAnd fs) = fs
conjuncts f = [f]

disjuncts :: FlatFormula a -> [FlatFormula a]
disjuncts (FlatOr fs) = fs
disjuncts f = [f]

-- | Sorts arguments of all commutative connectives recursively
sortFlatFormula :: Ord a => FlatFormula a -> FlatFormula a
sortFlatFormula f@(FlatAtomic _) = f
sortFlatFormula f@(FlatConst _) = f
sortFlatFormula (FlatNot f) = FlatNot (sortFlatFormula f)
sortFlatFormula (FlatAnd fs) = FlatAnd (sort (map sortFlatFormula fs))
sortFlatFormula (FlatOr  fs) = FlatOr  (sort (map sortFlatFormula fs))
sortFlatFormula (FlatImp f g) = FlatImp (sortFlatFormula f) (sortFlatFormula g)
sortFlatFormula (FlatIff f g) =
  let f' = sortFlatFormula f
      g' = sortFlatFormula g
  in if f' <= g'
     then FlatIff f' g'
     else FlatIff g' f'

-- | Replaces atoms by increasing integers (in order of occurrence)
normalizeAtoms :: Ord a => FlatFormula a -> FlatFormula Int
normalizeAtoms f = fmap (table Map.!) f
  where
    atoms = toList f
    distinctAtoms = nub atoms
    table = Map.fromList (zip distinctAtoms [0..])

-- | Replaces atoms by increasing integers (using an order isomorphism)
normalizeAtomsIso :: Ord a => FlatFormula a -> FlatFormula Int
normalizeAtomsIso f = fmap (table Map.!) f
  where
    atoms = toList f
    distinctAtoms = Set.toAscList (Set.fromList atoms)
    table = Map.fromList (zip distinctAtoms [0..])


data FlatFormula a
  = FlatAtomic !a
  | FlatConst !Bool
  | FlatNot (FlatFormula a)
  | FlatAnd [FlatFormula a]
  | FlatOr  [FlatFormula a]
  | FlatIff (FlatFormula a) (FlatFormula a)
  | FlatImp (FlatFormula a) (FlatFormula a)
  deriving (Show, Eq, Ord, Functor, Foldable)


prettyFlatFormula
  :: forall a ann.
     (Int -> a -> Doc ann)
  -> Int
  -> FlatFormula a
  -> Doc ann
prettyFlatFormula prettyAtom = go
  where
    go :: Int -> FlatFormula a -> Doc ann
    go _ (FlatConst False) = "False"
    go _ (FlatConst True) = "True"
    go prec (FlatAtomic x) = prettyAtom prec x
    go prec (FlatNot p) = bracket (prec > 10) 1 (prettyPrefix 10) "~" p
    go _    (FlatAnd []) = "True"
    go prec (FlatAnd [p]) = go prec p
    go prec (FlatAnd (p:ps)) = bracket (prec > 8) 0 (prettyInfix 8 "/\\") p (FlatAnd ps)
    go _    (FlatOr []) = "False"
    go prec (FlatOr [p]) = go prec p
    go prec (FlatOr (p:ps)) = bracket (prec > 6) 0 (prettyInfix 6 "\\/") p (FlatOr ps)
    go prec (FlatImp p q) = bracket (prec > 4) 0 (prettyInfix 4 "==>") p q
    go prec (FlatIff p q) = bracket (prec > 2) 0 (prettyInfix 2 "<=>") p q
    prettyPrefix :: Int -> Doc ann -> FlatFormula a -> Doc ann
    prettyPrefix newPrec sym p = sym <> go (newPrec+1) p  -- TODO: Remove +1 here to save parens on nested negations
    prettyInfix :: Int -> Doc ann -> FlatFormula a -> FlatFormula a -> Doc ann
    prettyInfix newPrec sym p q = go (newPrec+1) p <+> sym <> line <> go newPrec q
    bracket :: forall b c. Bool -> Int -> (b -> c -> Doc ann) -> b -> c -> Doc ann
    bracket br n f x y = (if br then parens else id) (nest n (align $ group $ f x y))

instance Pretty a => Pretty (FlatFormula a) where
  pretty = prettyFlatFormula (const pretty) 0
