{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Problems.SAT where

-- base
import Control.Monad
import Data.Foldable
import Data.Functor.Identity

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- directory
import System.Directory

-- filepath
import System.FilePath

-- list-transformer
import qualified List.Transformer as ListT

-- mtl
-- import Control.Monad.Reader
import Control.Monad.State.Strict

-- prettyprinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

-- QuickCheck
import Test.QuickCheck (Arbitrary(..), elements)

-- exagen
import Control.Monad.Choose
import Logic.Propositional.Formula hiding (Prop(..))
import Options (Options(..), SATOptions(..))


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
    go prec (Not f)   = bracket (prec > 10) $ goPrefix 10 "\\lnot " f
    go prec (And f g) = bracket (prec > 8) $ goInfix 8 " \\land " f g
    go prec (Or  f g) = bracket (prec > 8) $ goInfix 8 " \\lor " f g
    go prec (Iff f g) = bracket (prec > 8) $ goInfix 8 " \\leftrightarrow " f g
    go prec (Imp f g) = bracket (prec > 8) $ goInfix 8 " \\rightarrow " f g

    goPrefix prec sym f = sym <> go (prec+1) f

    goInfix prec sym f g = go (prec+1) f <> sym <> go (prec+1) g

    bracket True s = "( " <> s <> " )"
    bracket False s = s


-- TODO: add extra criterion to filter out formulas with too many parentheses in the rendered latex?
--       (visual complexity is probably a factor in how many mistakes people make)
main :: Options -> SATOptions -> IO ()
main Options{optNumExams,optOutputDir} SATOptions = do

  fms <- randomDistinctExamFormulas optNumExams

  case optOutputDir of
    Nothing -> do
      forM_ fms $ \fm -> do
        putStrLn $ showPretty fm
        -- putStrLn $ "Latex: " <> showLatex show fm
        -- putStrLn $ "Normalized: " <> showPretty (normalize fm)
        -- putStrLn $ "Normalizeds: " <> showPretty (sortFlatFormula (normalize fm))
        -- putStrLn $ "Proper Subformulas:\n" <> showPretty (properSubformulas fm)
        -- putStrLn $ "Polarities: " <> show (atomPolarity fm)
        -- putStrLn ""

    Just outputDir -> do
      outputDirExists <- doesDirectoryExist outputDir
      unless outputDirExists $
        error ("The given output directory does not exist: " <> show outputDir)

      forM_ (zip fms [1..]) $ \(fm, i :: Int) -> do
        let examDir = outputDir </> ("exam-" <> show i)
        createDirectoryIfMissing False examDir
        let file = examDir </> "sat.tex"
        putStrLn $ "Writing file: " <> file
        writeFile file ("\\[ " <> showLatex show fm <> " \\]\n")


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
  ]


suitable :: Ord a => Formula a -> Bool
suitable fm =
  foldl1 (&&)
  [ numAtoms fm == 3
  , -- there is at least one atom with pure polarity
    hasAtomPolarity Neg fm || hasAtomPolarity Pos fm
  , -- at least one but at most two equivalences
    let n = countSubformulas isIff fm
    in 1 <= n && n <= 2
  , anySubformula isImp fm
  , anySubformula isNot fm
  , anySubformula isAnd fm || anySubformula isOr fm
  , not (anySubformula isNestedNot fm)
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
  arbitrary = elements [P, Q, R]


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
  arbitrary = elements allPropPerms


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
      maybeFm <- evalRandomListIO' genExamFormula
      case maybeFm of
        Nothing -> error "empty sample space"
        Just fm | suitable fm -> return fm
                | otherwise -> go (n - 1)

-- NOTE: don't actually use this as it will keep the whole list in memory
-- allExamFormulas :: ListT Identity (Formula Prop)
-- allExamFormulas = filter suitable (genExamFormula @(ListT Identity))


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
      -- -- TODO: generates whole subformula before check
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
    isTrivial (And f g) = isLiteralWithSameAtom f g
    isTrivial (Or  f g) = isLiteralWithSameAtom f g
    isTrivial (Imp f g) = isLiteralWithSameAtom f g
    isTrivial (Iff f g) = isLiteralWithSameAtom f g
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
properSubformulas (Not g) = subformulas g
properSubformulas (And f g) = subformulas f ++ subformulas g
properSubformulas (Or  f g) = subformulas f ++ subformulas g
properSubformulas (Iff f g) = subformulas f ++ subformulas g
properSubformulas (Imp f g) = subformulas f ++ subformulas g


showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty layoutOptions . align . pretty
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }


hasAtomPolarity :: Ord a => Polarity -> Formula a -> Bool
hasAtomPolarity pol = (pol `elem`) . map snd . Map.toList . atomPolarity



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
polarity :: Formula a -> Formula (a, Polarity)
polarity = go Pos
  where
    go pol (Atomic x) = Atomic (x, pol)
    go pol (Not f) = Not (go (flipPolarity pol) f)
    go pol (And f g) = And (go pol f) (go pol g)
    go pol (Or  f g) = Or  (go pol f) (go pol g)
    go pol (Imp f g) = Imp (go (flipPolarity pol) f) (go pol g)
    go _   (Iff f g) = Iff (go Both f) (go Both g)

atomPolarity :: Ord a => Formula a -> Map a Polarity
atomPolarity = Map.fromListWith (<>) . toList . polarity
