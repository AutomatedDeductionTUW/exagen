{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Problems.Redundancy where

-- base
import Control.Exception
import Data.Foldable
import Data.List
import Data.Maybe
import Data.String

-- containers
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- filepath
import System.FilePath

-- lens
import Control.Lens

-- mtl
import Control.Monad.Reader

-- prettyprinter
import Data.Text.Prettyprint.Doc hiding (plural)

-- process
import System.Process

-- safe
import Safe

-- exagen
import Control.Monad.Choose
import Data.SExpr (Expr(..))
import Logic.FirstOrder
import Logic.SmtLib
import Options (Options(..), RedOptions(..))
import Text.Show.Latex
import Util


main :: Options -> RedOptions -> IO ()
main Options{optNumExams,optOutputDir,optSeed} RedOptions = do

  infs <-
    let normalize x = (fst x :: Inference PredSym FnSym Variable)
    in distinctRandomChoices' optNumExams normalize genExamInference
       >>= either fail pure

  assertM (length infs == optNumExams)

  forM_ (zip infs [1..]) $ \((inf, theta), i :: Int) -> do

    let content = mconcat
          [ "% Random number generator seed: ", show optSeed, "\n"
          , "% Index: ", show i, "\n"
          , "% Substitution: ", unwords (lines (showPretty theta)), "\n"
          , showLatex inf, "\n"
          ]

    let sigcontent = formatSignature inf

    case optOutputDir of
      Nothing -> do
        putStrLn "\nInference: "
        printPretty inf
        putStr "\nusing the substitution "
        printPretty theta
        -- putStrLn $ "Signature: " <> sigcontent

      Just outputDir -> do
        examDir <- getExamDir outputDir i
        let file = examDir </> "red.tex"
        let sigfile = examDir </> "red-signature.tex"
        putStrLn $ "Writing file: " <> file
        writeFile file content
        putStrLn $ "Writing file: " <> sigfile
        writeFile sigfile (sigcontent <> "\n")

        case premises inf of
          [mainPremise, sidePremise] -> do
            let sig = actualSignature inf
                objsort = "A"

            let soundness =
                  declareSignature objsort sig ++
                  [ assertClause objsort mainPremise
                  , assertClause objsort sidePremise
                  , assertNotClause objsort (conclusion inf)
                  , CheckSat
                  ]
            let soundnessFile = examDir </> "red-soundness.smt2"
            putStrLn $ "Writing file: " <> soundnessFile
            writeFile soundnessFile (formatSmtLib soundness)
            putStrLn $ "Checking soundness..."
            soundnessResult <- runVampire soundnessFile
            guard (soundnessResult == Unsatisfiable)

            let redundancy =  -- note: this is only the entailment part of the redundancy conditions
                  declareSignature objsort sig ++
                  [ assertClause objsort sidePremise
                  , assertClause objsort (conclusion inf)
                  , assertNotClause objsort mainPremise
                  , CheckSat
                  ]
            let redundancyFile = examDir </> "red-redundancy.smt2"
            putStrLn $ "Writing file: " <> redundancyFile
            writeFile redundancyFile (formatSmtLib redundancy)
            putStrLn $ "Checking redundancy..."
            redundancyResult <- runVampire redundancyFile
            guard (redundancyResult == Unsatisfiable)

          _ -> error "bug"



-- NOTE: I didn't want to implement a full parser for Vampire's output,
-- so we just recognize these cases for now
data SZSResult
  = Unsatisfiable
  | Other String
  deriving (Eq, Show)

runVampire :: FilePath -> IO SZSResult
runVampire path = do
  let options =
        [ "--input_syntax", "smtlib2"
        , "-t", "1"
        , "--proof", "off"
        , "-stat", "none"
        , path
        ]
  output <- readProcess "vampire" options ""
  let szsPrefix = "% SZS status "
      szsLines = filter (szsPrefix `isPrefixOf`) (lines output)
  case szsLines of
    [szsLine] ->
      case headDef "" . words . drop (length szsPrefix) $ szsLine of
        "Unsatisfiable" -> return Unsatisfiable
        other -> return (Other other)
    _ -> error "unknown"



formatSignature :: (HasSignature p fn v a, Ord v, ShowLatex p, ShowLatex fn, ShowLatex v) => a -> String
formatSignature x = formatSignature' (actualSignature x) (variablesOf x)

formatSignature' :: (ShowLatex p, ShowLatex fn, ShowLatex v) => Signature p fn -> Set v -> String
formatSignature' sig varsSet =
  formatDescription
  [ Descriptor (map showMath predicates) "predicate symbol"
  , Descriptor (map showMath functions) "function symbol"
  , Descriptor (map showMath constants) "constant"
  , Descriptor (map showMath vars) "variable"
  ]
  where
    showMath :: ShowLatex a => a -> String
    showMath x = "$" <> showLatex x <> "$"

    predicates = map symbol . Set.toAscList . sigPredicateSymbols $ sig
    functions = map symbol . filter ((>=1) . arity) . Set.toAscList . sigFunctionSymbols $ sig
    constants = map symbol . filter ((==0) . arity) . Set.toAscList . sigFunctionSymbols $ sig
    vars = Set.toAscList varsSet

data Descriptor = Descriptor
  { things :: [String]
  , what :: String
  }

formatDescription :: [Descriptor] -> String
formatDescription ds =
  case mapMaybe formatDescriptor ds of
    [] -> error "oh no"
    ds' -> oxfordComma ds' <> "."

formatDescriptor :: Descriptor -> Maybe String
formatDescriptor (Descriptor [] _) = Nothing
formatDescriptor (Descriptor [x] what) = Just $ x <> " is a " <> what
formatDescriptor (Descriptor xs what) = Just $ intercalate ", " xs <> " are " <> plural what

plural :: String -> String
plural what = what <> "s"

oxfordComma :: [String] -> String
oxfordComma [] = []
oxfordComma [x] = x
oxfordComma [x, y] = x <> " and " <> y
oxfordComma xs = intercalate ", " (init xs) <> ", and " <> last xs


-- | Generates an inference to be analysed in the exam problem.
--
-- Template: non-ground clause D
--           1 unit equality
--           1 non-equality literal
--
-- Generate: ground instance C of D
--           plus one extra literal (maybe of the non-equality literal)
--
--    C    D
--  ----------
--      ?
--
--  (note that the main premise is the left one!)
--
-- Questions:
-- a) Is the inference sound? prove using Sup+BR
-- b) Is the inference simplifying?
genExamInference
  :: MonadChoose m
  => m ( Inference PredSym FnSym Variable
       , Substitution FnSym Variable
       )
genExamInference = do

  let sigFns = Set.fromList @(Symbol FnSym)
        [ Symbol "f" 1
        , Symbol "g" 2
        , Symbol "h" 1
        , Symbol "a" 0
        , Symbol "b" 0
        , Symbol "c" 0
        , Symbol "d" 0
        ]
      sigPreds = Set.fromList @(Symbol PredSym)
        [ Symbol "P" 1
        ]
      sig = Signature { sigFunctionSymbols = sigFns
                      , sigPredicateSymbols = sigPreds
                      }
      -- vars = [minBound .. maxBound] :: [Variable]
      vars = [X, Y]
      opts = GenOptions{ minDepth = 1
                       , maxDepth = 2
                       , sig = sig
                       , vars = vars
                       }

  let fnsWithArity2 = Set.map symbol $ Set.filter ((==2) . arity) sigFns

  -- Exactly one variable in each non-ground literal, and they should be different
  v1 <- choose vars
  v2 <- choose (filter (/= v1) vars)

  -- Generate literals
  -- l1: non-ground uninterpreted literal
  -- l2: ground uninterpreted literal
  -- l3: non-ground equality literal
  l1 <- mfilter (not . isGround)
        . mfilter ((==1) . length . toListOf variables)  -- exactly one variable occurrence
        $ genUninterpretedLiteral opts{ vars = [v1] }
  l2 <- genUninterpretedLiteral opts{ vars = [] }
  l3 <- mfilter (not . isGround)
        . mfilter ((>=2) . length . toListOf variables)  -- at least two variable occurrences
        $ genEqualityLiteral opts{ vars = [v2] }

  -- At least one function of arity two should appear
  let fns = functionSymbolsOf (Clause [l1, l2, l3])
  guard (not . Set.null $ Set.intersection fns fnsWithArity2)

  -- Generate ground substitution
  t1 <- genTerm GenOptions{ minDepth = 1
                          , maxDepth = 1
                          , sig = sig
                          , vars = []
                          }
  t2 <- genTerm GenOptions{ minDepth = 0
                          , maxDepth = 0
                          , sig = sig
                          , vars = []
                          }
  let theta = Substitution $ Map.fromList [ (v1, t1), (v2, t2) ]

  -- Clauses
  -- c1: left premise, ground, redundant after inference, one additional irrelevant literal
  -- c2: right premise, non-ground
  -- c3: conclusion after applying subsumption resolution
  let mainPremise = Clause [applySubstitution theta (complementary l1), l2, applySubstitution theta l3]
  let sidePremise = Clause [l1, l3]
  let conclusion = Clause [l2, applySubstitution theta l3]
  -- TODO: might also permute the order of literals

  -- TODO
  -- * Idea: Control total number of symbols in term? [ not exactly, but in narrow range ]
  --         So if someone gets less binary functions, they will have more nesting instead.
  let inf = Inference{ premises = [ mainPremise, sidePremise ]
                     , conclusion = conclusion
                     }

  {-
  let renameVariable = (inf ^.. variables) `normalizeTo` [minBound..maxBound]
      renameConstantSymbol = (inf ^.. constantSymbols) `normalizeTo` ["a", "b", "c", "d"]

      rename :: HasTerms' FnSym Variable a => a -> a
      rename = over variables renameVariable
               . over constantSymbols renameConstantSymbol

      inf' = rename inf

      theta' =
        Substitution . Map.fromList $
        [ (renameVariable v, rename (getSubstitution theta Map.! v)) | v <- inf ^.. variables ]
  -}

  return (inf, theta)


normalizeTo :: Ord a => [a] -> [a] -> (a -> a)
domain `normalizeTo` values = \x -> fromMaybe x (lookup x table)
  where
    domain' = Set.toList (Set.fromList domain)  -- de-duplicate and sort
    table = assert (length values >= length domain') $
            zip domain' values


-- | Concrete type for predicate symbols
newtype PredSym = PredSym String
  deriving newtype (Eq, Ord, Show, IsString, Pretty, ShowLatex, ToSmtLib)

-- | Concrete type for function symbols (including constant symbols)
newtype FnSym = FnSym String
  deriving newtype (Eq, Ord, Show, IsString, Pretty, ShowLatex, ToSmtLib)

-- | Concrete type for variables
data Variable = X | Y | Z
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty Variable where
  pretty X = "x"
  pretty Y = "y"
  pretty Z = "z"

instance ShowLatex Variable where
  showLatex X = "x"
  showLatex Y = "y"
  showLatex Z = "z"

instance ToSmtLib Variable where
  toSmtLib X = Value "x"
  toSmtLib Y = Value "y"
  toSmtLib Z = Value "z"


-- | Options for generators (mostly for terms).
--
-- depth: number of nested function applications in generated terms (predicates aren't counted here).
-- weight: total number of symbols in generated terms (the predicate isn't counted).
-- (TODO: weight restrictions not implemented yet)
data GenOptions p fn v = GenOptions
  { minDepth :: Int
  , maxDepth :: Int
  -- , minWeight :: Int
  -- , maxWeight :: Int
  , sig :: Signature p fn
  , vars :: [v]
  }

data TermCtx = TermCtx
  { _depth :: Int   -- starts at 0 for the whole term, add 1 each time we go into a term's arguments
  -- , _weight :: Int   -- the exact weight we want this subterm to have
  }

depth :: Lens' TermCtx Int
depth = lens _depth (\x y -> x{ _depth = y })

-- weight :: Lens' TermCtx Int
-- weight = lens _weight (\x y -> x{ _weight = y })

genTerm :: forall m p fn v. MonadChoose m => GenOptions p fn v -> m (Term fn v)
genTerm GenOptions{minDepth,maxDepth,{- minWeight,maxWeight,-} sig,vars} = do
  -- actualWeight <- choose [minWeight .. maxWeight]
  let initialCtx = TermCtx
        { _depth = 0
        -- , _weight = actualWeight
        }
  runReaderT term initialCtx

  where
    constants = filter ((==0) . arity) . Set.toList . sigFunctionSymbols $ sig
    functions = filter ((>0) . arity) . Set.toList . sigFunctionSymbols $ sig

    term :: ReaderT TermCtx m (Term fn v)
    term = do
      d <- view depth
      assertM (minDepth <= maxDepth)  -- not really necessary, we just don't return any terms in that case

      gen <- choose $ concat
             [ if d < maxDepth then [fnApp] else []
             , if d >= minDepth then [var, constApp] else []
             ]
      gen

    fnApp = do
      f <- choose functions
      args <- local (over depth (+1)) $ replicateM (arity f) term
      return (App (symbol f) args)

    constApp = do
      c <- choose constants
      return (App (symbol c) [])

    var = Var <$> choose vars


genLiteral' :: forall m p fn v. MonadChoose m => m (Atom p fn v) -> m (Literal p fn v)
genLiteral' genAtom = do
  atom <- genAtom
  pos <- choose [True, False]
  return (Literal pos atom)


genUninterpretedLiteral :: forall m p fn v. MonadChoose m => GenOptions p fn v -> m (Literal p fn v)
genUninterpretedLiteral opts = genLiteral' (genUninterpretedAtom opts)


genEqualityLiteral :: forall m p fn v. MonadChoose m => GenOptions p fn v -> m (Literal p fn v)
genEqualityLiteral opts = genLiteral' (genEqualityAtom opts)


genUninterpretedAtom :: forall m p fn v. MonadChoose m => GenOptions p fn v -> m (Atom p fn v)
genUninterpretedAtom opts@GenOptions{sig} = do
  let predicates = Set.toList (sigPredicateSymbols sig)
  p <- choose predicates
  args <- replicateM (arity p) (genTerm opts)
  return (Uninterpreted (symbol p) args)


genEqualityAtom :: forall m p fn v. MonadChoose m => GenOptions p fn v -> m (Atom p fn v)
genEqualityAtom opts = do
  t1 <- genTerm opts
  t2 <- genTerm opts
  return (Equality t1 t2)


genSubstitution
  :: forall m p fn v. (MonadChoose m, Ord v)
  => GenOptions p fn v   -- ^ options control the generation of terms in the range
  -> [v]   -- ^ domain of the generated substitution (will be identity on all other values)
  -> m (Substitution fn v)
genSubstitution opts domain = do
  pairs <- traverse (\v -> fmap (v,) (genTerm opts)) domain
  return $ Substitution (Map.fromList pairs)
