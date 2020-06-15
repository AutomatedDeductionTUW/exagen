{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Problems.SAT where

-- base
import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.List
import Data.Semigroup

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- filepath
import System.FilePath

-- lens
import Control.Lens hiding (Const)

-- list-transformer
import qualified List.Transformer as ListT

-- mtl
-- import Control.Monad.Reader
import Control.Monad.State.Strict

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- QuickCheck
import Test.QuickCheck (Arbitrary(..), elements)

-- exagen
import Control.Monad.Choose
import Logic.DefinitionalNF
import Logic.Formula hiding (Prop(..))
import Options (Options(..), SATOptions(..))
import Util


-- For the exam we just add all parentheses except
-- * the outer ones around the whole formula
-- * the ones around negations
showLatex :: (a -> String) -> Formula a -> String
showLatex atomToLatex = go (0 :: Int)
  where
    -- prec: the precedence of the operator in the parent node
    --       => if it is higher than the precedence of the current node,
    --          we have to add parentheses to keep the same parse tree
    go _ (Atomic a) = atomToLatex a
    go _ (Const True) = " \\top "
    go _ (Const False) = " \\bot "
    go prec (Not f)   = bracket (prec > 10) $ goPrefix 10 "\\lnot " f
    go prec (And f g) = bracket (prec > 8) $ goInfix 8 " \\land " f g
    go prec (Or  f g) = bracket (prec > 8) $ goInfix 8 " \\lor " f g
    go prec (Iff f g) = bracket (prec > 8) $ goInfix 8 " \\leftrightarrow " f g
    go prec (Imp f g) = bracket (prec > 8) $ goInfix 8 " \\rightarrow " f g

    goPrefix prec sym f = sym <> go (prec+1) f

    goInfix prec sym f g = go (prec+1) f <> sym <> go (prec+1) g

    bracket True s = "( " <> s <> " )"
    bracket False s = s


nestedLatexParens :: forall a. Formula a -> Int
nestedLatexParens = getMax . go (0 :: Int)
  where
    -- prec: the precedence of the operator in the parent node
    --       => if it is higher than the precedence of the current node,
    --          we have to add parentheses to keep the same parse tree
    go :: Int -> Formula a -> Max Int
    go _ (Atomic _) = 0
    go _ (Const _) = 0
    go prec (Not f)   = bracket (prec > 10) $ goPrefix 10 f
    go prec (And f g) = bracket (prec > 8) $ goInfix 8 f g
    go prec (Or  f g) = bracket (prec > 8) $ goInfix 8 f g
    go prec (Iff f g) = bracket (prec > 8) $ goInfix 8 f g
    go prec (Imp f g) = bracket (prec > 8) $ goInfix 8 f g

    goPrefix prec f = go (prec+1) f

    goInfix prec f g = go (prec+1) f <> go (prec+1) g

    bracket True = (+1)
    bracket False = id



main :: Options -> SATOptions -> IO ()
main Options{optNumExams,optOutputDir,optSeed} SATOptions = do

  fms <- randomDistinctExamFormulas optNumExams

  case optOutputDir of
    Nothing -> do
      forM_ fms $ \fm -> do
        putStrLn $ showPretty fm
        putStrLn $ "Satisfiable? " <> show (satisfiable fm)
        putStrLn $ "Valid? " <> show (valid fm)
        -- putStrLn $ "Latex: " <> showLatex show fm
        -- putStrLn $ "Normalized: " <> showPretty (normalize fm)
        -- putStrLn $ "Normalizeds: " <> showPretty (sortFlatFormula (normalize fm))
        -- putStrLn $ "Proper Subformulas:\n" <> showPretty (properSubformulas fm)
        putStrLn $ "Polarities: " <> show (atomPolarity fm)
        let (p, defs) = definitionalNF' fm
            allDefs = Atomic p : defs
        putStrLn $ "Definitional transformation:"
        putDocLn $ vsep (map (indent 4 . pretty) allDefs)
        putStr $ "Models: "
        putDocLn $ list (prettyAssignment <$> models fm)
        putStrLn ""

    Just outputDir -> do
      forM_ (zip fms [1..]) $ \(fm, i :: Int) -> do
        examDir <- getExamDir outputDir i
        let file = examDir </> "sat.tex"
        putStrLn $ "Writing file: " <> file
        let content = mconcat
              [ "% Random number generator seed: ", show optSeed, "\n"
              , "% Index: ", show i, "\n"
              , "% Satisfiable? ", show (satisfiable fm), "\n"
              , "% Valid? ", show (valid fm), "\n"
              , "% Polarities: ", show (atomPolarity fm), "\n"
              , "% Models: ", showDoc $ list (prettyAssignment <$> models fm), "\n"
              , showLatex show fm <> "\n"
              ]
        writeFile file content


prettyAssignment :: (Ord a, Pretty a) => Assignment a -> Doc ann
prettyAssignment xs = encloseSep lbrace rbrace comma (map pretty1 $ sort xs)
  where pretty1 (x, True) = pretty x
        pretty1 (x, False) = "¬" <> pretty x


-- Results for size = 5:
-- Size of suitable sample space: 450240
-- Size of sample space: 1767744
--
-- Results for size = 6:
-- Size of suitable sample space: 22999440
-- Size of sample space: 96837120
--
-- Results for size = 7:
-- (aborted because it takes too long)
enumerateSampleSpace :: IO ()
enumerateSampleSpace = do
  -- putStrLn $ "Size of sample space: " <> show (length @[] genExamFormula)
  let Pair numFormulas numSuitableFormulas :: Pair Int Int =
        runIdentity $
        ListT.fold (\(Pair n k) fm -> Pair (n+1) (if suitableFast fm then k+1 else k)) (Pair 0 0) id $
        genExamFormula
  putStrLn $ "Size of suitable sample space: " <> show numSuitableFormulas
  putStrLn $ "Size of sample space: " <> show numFormulas

-- | 'Pair a b' is a strict pair, as opposed to the built-in tuple '(a,b)', which is lazy.
data Pair a b = Pair !a !b
  deriving (Eq, Ord, Show)


-- Only checks those criteria from 'suitable' that haven't been inlined into the generator
suitableFast :: Ord a => Formula a -> Bool
suitableFast fm =
  foldl1 (&&)
  [ numAtoms fm == 3
  , -- there is at least one atom with pure polarity
    hasAtomPolarity Neg fm || hasAtomPolarity Pos fm
  , not (anySubformula isNestedNot fm)
  , nestedLatexParens fm < 3
  , length (models fm) <= 6
  , hasVarietyInDefinitionalNF fm
  -- , length (models fm) >= 2
  -- , -- there is exactly one model
  --   lengthIs 1 (models fm)
  -- , -- there is at most one model
  --   -- (rationale: many models -> high probability that no branching is required in solution,
  --   -- but also not all should be unsat so it's not too predictable.)
  --   null . tailSafe . models $ fm
  ]


suitable :: Ord a => Formula a -> Bool
suitable fm =
  foldl1 (&&)
  [ suitableFast fm
  , -- at least one but at most two equivalences
    let n = countSubformulas isIff fm
    in 1 <= n && n <= 2
  , anySubformula isImp fm
  , anySubformula isNot fm
  , anySubformula isAnd fm || anySubformula isOr fm
  , not (anySubformula isTrivialBinaryConnective fm)
  ]


data Prop = P | Q | R
  deriving (Eq, Ord)

instance Show Prop where
  show P = "p"
  show Q = "q"
  show R = "r"

instance Pretty Prop where
  pretty = pretty . show

instance Arbitrary Prop where
  arbitrary = Test.QuickCheck.elements [P, Q, R]


-- | Extend 'Prop' with a supply of new names to allow definitional transformation
data Prop' a
  = OriginalProp !a
  | NewProp !Word
  deriving (Eq, Ord)

instance Show a => Show (Prop' a) where
  show (OriginalProp p) = show p
  show (NewProp i) = 'n' : show i

instance Show a => Pretty (Prop' a) where
  pretty = pretty . show

newPropIndex :: Traversal' (Prop' a) Word
newPropIndex handler (NewProp i) = NewProp <$> handler i
newPropIndex _ p = pure p

newProps :: Stream (Prop' a)
newProps = go 1
  where go !i = NewProp i ::: go (i+1)

definitionalNF' :: Ord a => Formula a -> (Prop' a, [Formula (Prop' a)])
definitionalNF' = reverseNewProps . definitionalNF newProps . fmap OriginalProp

-- reverse the indices of new names (to match what we got in the lecture)
reverseNewProps :: (Prop' a, [Formula (Prop' a)]) -> (Prop' a, [Formula (Prop' a)])
reverseNewProps defs =
  over (prop . newPropIndex) (\i -> h - i)
  . over _2 reverse
  $ defs
  where
    h = 1 + getMax (defs ^. prop . newPropIndex . to Max)

    prop :: Traversal (a, [Formula a]) (b, [Formula b]) a b
    prop = tpair id (traverse . traverse)

-- | Check whether the formula has at least two different types of definitions
-- in its definitional normal form.
-- (types arise form polarity optimisation and are "n ==> F", "F ==> n", "n <=> F")
hasVarietyInDefinitionalNF :: Ord a => Formula a -> Bool
hasVarietyInDefinitionalNF = (>=2) . Set.size . Set.fromList . map defType . snd . definitionalNF'
  where
    defType (Imp (Atomic (NewProp _)) _) = 1 :: Int
    defType (Imp _ (Atomic (NewProp _))) = 2
    defType (Iff (Atomic (NewProp _)) _) = 3
    defType _ = error "bug: unexpected definition"


-- | Permutation of 'Prop' values
data PropPerm = PropPerm
  { permName :: String
  , applyPerm :: Prop -> Prop
  }

allPropPerms :: [PropPerm]
allPropPerms =
  [ PropPerm "PQR" id
  , PropPerm "QPR" pq
  , PropPerm "RQP" pr
  , PropPerm "PRQ" qr
  , PropPerm "QRP" (pr . pq)
  , PropPerm "RPQ" (qr . pq)
  ]
  where
    pq P = Q
    pq Q = P
    pq x = x
    pr P = R
    pr R = P
    pr x = x
    qr Q = R
    qr R = Q
    qr x = x

instance Show PropPerm where
  show = permName

instance Arbitrary PropPerm where
  arbitrary = Test.QuickCheck.elements allPropPerms


randomDistinctExamFormulas :: Int -> IO [Formula Prop]
randomDistinctExamFormulas = fmap reverse . go [] Set.empty
  where
    go
      :: [Formula Prop]  -- ^ the formulas generated so far
      -> Set (FlatFormula Int)  -- ^ the formulas generated so far, in normalized form
      -> Int  -- ^ how many do we still have to generate
      -> IO [Formula Prop]
    go fs _ 0 = return fs
    go fs normalizedFormulas n = do
      f <- randomExamFormula
      let fperms = [ applyPerm p <$> f | p <- allPropPerms ]
          nfperms = Set.fromList (normalize <$> fperms)
      if any (`Set.member` normalizedFormulas) nfperms
        then go fs normalizedFormulas n
        else go (f:fs) (normalizedFormulas `Set.union` nfperms) (n - 1)


randomExamFormula :: IO (Formula Prop)
randomExamFormula = go 1000
  where
    go :: Int -> IO (Formula Prop)
    go 0 = error "maximum number of tries exceeded"
    go n = do
      maybeFm <- evalRandomChoiceIO genExamFormula
      case maybeFm of
        Nothing -> error "empty sample space"
        Just fm | suitable fm -> return fm
                | otherwise -> go (n - 1)


genExamProp :: MonadChoose m => m Prop
genExamProp = choose [P, Q, R]


genExamFormula :: MonadChoose m => m (Formula Prop)
genExamFormula = evalStateT (genFormulaPruned size genExamProp) initialState
  where
    size = 7
    initialState = GenState
      { requiredConnectives = [[CImp], [CIff], [CNot], [CAnd, COr]]
      , remainingConnectives = size
      , allowedIff = 2
      }

data Connective = CNot | CAnd | COr | CImp | CIff
  deriving (Eq, Ord, Show)

type GenT m = StateT GenState m

data GenState = GenState
  { -- NOTE: in current implementation, all constraints (type [Connective]) must be pairwise disjoint!
    requiredConnectives :: [[Connective]]
  , -- How many connectives are still to be generated in the whole formula (not the subformula size)
    remainingConnectives :: Int
  , -- How many equivalences are still allowed in the whole formula
    allowedIff :: Int
  }

genFormulaPruned :: forall m a. (Eq a, MonadChoose m) => Int -> GenT m a -> GenT m (Formula a)
genFormulaPruned totalSize genProp = formula totalSize
  where
    formula :: Int -> GenT m (Formula a)
    formula 0 = base
    formula n = recursive n

    base :: GenT m (Formula a)
    base = Atomic <$> genProp

    recursive :: Int -> GenT m (Formula a)
    recursive n = do
      numRemaining <- gets remainingConnectives
      numConstraints <- gets (length . requiredConnectives)
      when (numRemaining < numConstraints) $ error "bug: remaining < constraints"
      availableConnectives <-
        if numRemaining <= numConstraints
        then concat <$> gets requiredConnectives
        else do numAllowedIff <- gets allowedIff
                if numAllowedIff > 0
                  then return [CNot, CAnd, COr, CImp, CIff]
                  else return [CNot, CAnd, COr, CImp]
      conn <-
        choose availableConnectives
      modify' $ \s -> s{ requiredConnectives = filter (all (/=conn)) (requiredConnectives s)
                       , remainingConnectives = remainingConnectives s - 1
                       , allowedIff = allowedIff s - (if conn == CIff then 1 else 0)
                       }

      case conn of
        CNot -> unaryConnective n Not
        CAnd -> binaryConnective n And
        COr  -> binaryConnective n Or
        CImp -> binaryConnective n Imp
        CIff -> binaryConnective n Iff

    unaryConnective :: Int -> (Formula a -> Formula a) -> GenT m (Formula a)
    unaryConnective n f = do
      inner <- formula (n - 1)
      -- let fm = f inner
      -- NOTE: this wouldn't help as it generates the whole subformula before the check
      -- if isNestedNot fm
      --   then mzero
      --   else return fm
      return (f inner)

    binaryConnective :: Int -> (Formula a -> Formula a -> Formula a) -> GenT m (Formula a)
    binaryConnective n f = do
      let n' = n - 1
      leftSize <- choose [0 .. n']
      let rightSize = n' - leftSize
      left <- formula leftSize
      right <- formula rightSize
      let fm = f left right
      if isTrivialBinaryConnective fm
        then mzero
        else return fm


-- Return true for binary connectives where both arguments are a literal using the same atom.
-- For example, things like (p /\ ~p), (p \/ p), ...
isTrivialBinaryConnective :: Eq a => Formula a -> Bool
isTrivialBinaryConnective = isTrivial
  where
    isTrivial (Binary _ f g) = isLiteralWithSameAtom f g
    isTrivial _ = False

    isLiteralWithSameAtom f g =
      case (getAtom f, getAtom g) of
        (Just x, Just y) -> x == y
        _ -> False

    getAtom :: Formula a -> Maybe a
    getAtom (Atomic x) = Just x
    getAtom (Not (Atomic x)) = Just x
    getAtom _ = Nothing


anySubformula :: (Formula a -> Bool) -> Formula a -> Bool
anySubformula p = any p . subformulas

countSubformulas :: (Formula a -> Bool) -> Formula a -> Int
countSubformulas p = length . filter p . subformulas

isNestedNot :: Formula a -> Bool
isNestedNot (Not (Not _)) = True
isNestedNot _ = False

isNot :: Formula a -> Bool
isNot (Not _) = True
isNot _ = False

isAnd :: Formula a -> Bool
isAnd (And _ _) = True
isAnd _ = False

isOr :: Formula a -> Bool
isOr (Or _ _) = True
isOr _ = False

isImp :: Formula a -> Bool
isImp (Imp _ _) = True
isImp _ = False

isIff :: Formula a -> Bool
isIff (Iff _ _) = True
isIff _ = False

-- Number of different atoms that occur in the formula
numAtoms :: Ord a => Formula a -> Int
numAtoms = Set.size . Set.fromList . toList

subformulas :: Formula a -> [Formula a]
subformulas f = f : properSubformulas f

properSubformulas :: Formula a -> [Formula a]
properSubformulas (Atomic _) = []
properSubformulas (Const _) = []
properSubformulas (Not g) = subformulas g
properSubformulas (Binary _ f g) = subformulas f ++ subformulas g
