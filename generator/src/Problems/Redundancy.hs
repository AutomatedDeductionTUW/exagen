{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Problems.Redundancy where

-- base
import Control.Applicative
import Control.Exception (assert)
import Control.Monad (ap)
import Control.Monad.Fail
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Void

-- containers
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens

-- mtl
import Control.Monad.Reader

-- prettyprinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

-- random
import System.Random (StdGen)

-- exagen
import Control.Monad.Choose
import Options (Options(..), RedOptions(..))
import Text.Show.Latex





main :: Options -> RedOptions -> IO ()
main Options{optNumExams,optOutputDir,optSeed} RedOptions = do

  -- TODO
  -- Criteria:
  -- * Exactly one variable in each non-ground literal, and they should be different
  -- * At least one function of arity two should appear
  -- * Idea: Control total number of symbols in term? [ not exactly, but in narrow range ]
  --         So if someone gets more unary functions, they will have more nesting instead.

  inf <- randomExamInference
  putStrLn "\nInference: "
  printPretty inf
  putStrLn $ "\n" ++ showLatex inf

-- TODO:
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


newtype PredSym = PredSym String
  deriving newtype (Eq, Ord, Show, IsString, Pretty, ShowLatex)

newtype FnSym = FnSym String
  deriving newtype (Eq, Ord, Show, IsString, Pretty, ShowLatex)


randomExamInference :: IO (Inference PredSym FnSym Variable)
randomExamInference = do
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
      vars = [minBound .. maxBound] :: [Variable]
      opts = GenOptions{ minDepth = 2
                       , maxDepth = 3
                       , sig = sig
                       , vars = vars
                       }

  -- Generate literals
  -- l1: non-ground uninterpreted literal
  -- l2: ground uninterpreted literal
  -- l3: non-ground equality literal
  l1 <- randomGen (filterChoiceFail isNonGroundLiteral $ genUninterpretedLiteral opts)
  l2 <- randomGen (genUninterpretedLiteral opts{ vars = [] @Variable })
  l3 <- randomGen (filterChoiceFail isNonGroundLiteral $ genEqualityLiteral opts)

  -- Generate ground substitution
  let thetaOpts = GenOptions{ minDepth = 0
                            , maxDepth = 1
                            , sig = sig
                            , vars = [] @Variable
                            }
  theta <- randomGen (genSubstitution thetaOpts vars)

  -- Clauses
  -- c1: left premise, ground, redundant after inference, one additional irrelevant literal
  -- c2: right premise, non-ground
  -- c3: conclusion after applying subsumption resolution
  let c1 = Clause [applySubstitutionL theta (complementary l1), l2, applySubstitutionL theta l3]
  let c2 = Clause [l1, l3]
  let c3 = Clause [l2, applySubstitutionL theta l3]

  -- TODO
  -- Criteria:
  -- * Exactly one variable in each non-ground literal, and they should be different
  -- * At least one function of arity two should appear
  -- * Idea: Control total number of symbols in term? [ not exactly, but in narrow range ]
  --         So if someone gets more unary functions, they will have more nesting instead.
  let inf = Inference{ premises = [ c1, c2 ]
                     , conclusion = c3
                     }
  return inf




data Inference p fn v = Inference
  { premises :: [Clause p fn v]
  , conclusion :: Clause p fn v
  }
  deriving (Eq, Ord, Show)

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Inference p fn v) where
  pretty Inference{premises,conclusion} =
    vsep (pretty <$> premises) <> hardline
    <> pretty (replicate 50 '-') <> hardline
    <> pretty conclusion

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Inference p fn v) where
  showLatex Inference{premises,conclusion} =
    "\\prftree" <> concat (wrap . showLatex <$> premises) <> wrap (showLatex conclusion)
    where
      wrap s = '{' : s ++ "}"


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


-- type Substitution fn v = v -> Term fn v

-- use map instead of function so we can print it
newtype Substitution fn v =
  Substitution { unSubstitution :: Map v (Term fn v) }
  deriving (Eq, Ord, Show)

instance (Pretty fn, Pretty v) => Pretty (Substitution fn v) where
  pretty (Substitution s) = encloseSep lbrace rbrace comma (map prettyPair pairs)
    where
      pairs = Map.toList s
      prettyPair (v, t) = pretty v <+> "->" <+> pretty t

applySubstitution :: Ord v => Substitution fn v -> Term fn v -> Term fn v
applySubstitution (Substitution s) t = t >>= (\v -> fromMaybe (Var v) (Map.lookup v s))

applySubstitutionA :: Ord v => Substitution fn v -> Atom p fn v -> Atom p  fn v
applySubstitutionA theta (Equality t1 t2) = Equality (applySubstitution theta t1) (applySubstitution theta t2)
applySubstitutionA theta (Uninterpreted p ts) = Uninterpreted p (applySubstitution theta <$> ts)

applySubstitutionL :: Ord v => Substitution fn v -> Literal p fn v -> Literal p  fn v
applySubstitutionL theta (Literal pos atom) = Literal pos (applySubstitutionA theta atom)


isNonGroundLiteral :: Literal p fn v -> Bool
isNonGroundLiteral = getAny . foldMap (const (Any True))

complementary :: Literal p fn v -> Literal p fn v
complementary (Literal pos a) = Literal (not pos) a



-- TODO: We probably don't need this here
data KBOParams fn = KBOParams
  { precedence :: [fn]
  , weights :: Map fn Int
  , variableWeight :: Int
  }



newtype Clause p fn v = Clause { literals :: [Literal p fn v] }
  deriving (Eq, Ord, Show, Functor, Foldable)

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Clause p fn v) where
  pretty (Clause []) = "⚡️"
  pretty (Clause (l:ls)) = pretty l <+> nest 4 (fillSep (map (\x -> "\\/" <+> pretty x) ls))

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Clause p fn v) where
  showLatex (Clause []) = " \\Box "
  showLatex (Clause ls) = intercalate " \\lor " (map showLatex ls)


data Literal p fn v = Literal
  { isPositive :: Bool
  , atom :: Atom p fn v
  }
  deriving (Eq, Ord, Show, Functor, Foldable)

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Literal p fn v) where
  pretty (Literal True (Equality t1 t2)) = pretty t1 <+> "=" <+> pretty t2
  pretty (Literal False (Equality t1 t2)) = pretty t1 <+> "≠" <+> pretty t2
  pretty (Literal True a) = pretty a
  pretty (Literal False a) = "¬" <> pretty a

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Literal p fn v) where
  showLatex (Literal True (Equality t1 t2)) = showLatex t1 <> " = " <> showLatex t2
  showLatex (Literal False (Equality t1 t2)) = showLatex t1 <> " \\neq " <> showLatex t2
  showLatex (Literal True a) = showLatex a
  showLatex (Literal False a) = " \\lnot " <> showLatex a


data Atom p fn v
  = Equality (Term fn v) (Term fn v)
  | Uninterpreted p [Term fn v]
  deriving (Eq, Ord, Show, Functor, Foldable)

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Atom p fn v) where
  pretty (Equality t1 t2) = pretty t1 <+> "=" <+> pretty t2
  pretty (Uninterpreted p args) = pretty p <> align (encloseSep lparen rparen comma (map pretty args))

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Atom p fn v) where
  showLatex (Equality t1 t2) = showLatex t1 <> " = " <> showLatex t2
  showLatex (Uninterpreted p args) = showLatex p <> " ( " <> intercalate " , " (map showLatex args) <> " )"


data Term fn v
  = Var v
  | App fn [Term fn v]
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative (Term fn) where
  pure = Var
  (<*>) = ap

instance Monad (Term fn) where
  t >>= f = joinTerm (fmap f t)


joinTerm :: Term fn (Term fn v) -> Term fn v
joinTerm (Var t) = t
joinTerm (App fn ts) = App fn (joinTerm <$> ts)


instance (Pretty fn, Pretty v) => Pretty (Term fn v) where
  pretty (Var v) = pretty v
  pretty (App c []) = pretty c
  pretty (App fn args) = pretty fn <> align (encloseSep lparen rparen comma (map pretty args))

instance (ShowLatex fn, ShowLatex v) => ShowLatex (Term fn v) where
  showLatex (Var v) = showLatex v
  showLatex (App c []) = showLatex c
  showLatex (App fn args) = showLatex fn <> " ( " <> intercalate " , " (map showLatex args) <> " )"


data Symbol a = Symbol
  { symbol :: !a
  , arity :: !Int
  }
  deriving (Eq, Ord, Show, Functor)


data Signature p fn = Signature
  { sigFunctionSymbols :: Set (Symbol fn)
  , sigPredicateSymbols :: Set (Symbol p)
  }


randomGen :: RandomGen a -> IO a
randomGen g = do
  mx <- evalRandomListIO' $ runGen g
  case mx of
    Just x -> return x
    Nothing -> error "empty sample space"


genGroundTerm :: MonadChoose m => GenOptions p fn v -> GenT m (Term fn Void)
genGroundTerm opts = genTerm opts'
  where opts' = opts{ vars = [] :: [Void] }


runGen :: GenT m a -> m a
runGen g = runReaderT (unGenT g) initialCtx
  where
    initialCtx = GenCtx
      { _depth = 0
      }

data GenOptions p fn v = GenOptions
  { minDepth :: Int
  , maxDepth :: Int
  , sig :: Signature p fn
  , vars :: [v]
  }

newtype GenT m a = GenT { unGenT :: ReaderT GenCtx m a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail)
  deriving newtype (MonadPlus, MonadChoose, MonadReader GenCtx)

type RandomGen = GenT (RandomListT StdGen Identity)

data GenCtx = GenCtx
  { _depth :: Int   -- starts at 0 for the whole term, +1 each time we go into a term's arguments
  }

depth :: Lens' GenCtx Int
depth = lens _depth (\x y -> x{ _depth = y })

genTerm :: forall m p fn v. MonadChoose m => GenOptions p fn v -> GenT m (Term fn v)
genTerm GenOptions{minDepth,maxDepth,sig,vars} = term
  where
    constantSymbols = filter ((==0) . arity) . Set.toList . sigFunctionSymbols $ sig
    functionSymbols = filter ((>0) . arity) . Set.toList . sigFunctionSymbols $ sig

    term :: GenT m (Term fn v)
    term = do
      d <- view depth
      assertM (minDepth <= maxDepth)  -- not really necessary, we just don't return any terms in that case

      gen <- choose $ concat
             [ if d < maxDepth then [fnApp] else []
             , if d >= minDepth then [var, constApp] else []
             ]
      gen

    fnApp :: GenT m (Term fn v)
    fnApp = do
      f <- choose functionSymbols
      args <- local (over depth (+1)) $ replicateM (arity f) term
      return (App (symbol f) args)

    constApp :: GenT m (Term fn v)
    constApp = do
      c <- choose constantSymbols
      return (App (symbol c) [])

    var :: GenT m (Term fn v)
    var = Var <$> choose vars


genLiteral' :: forall m p fn v. MonadChoose m => GenT m (Atom p fn v) -> GenT m (Literal p fn v)
genLiteral' genAtom = do
  atom <- genAtom
  pos <- choose [True, False]
  return (Literal pos atom)

genUninterpretedLiteral :: forall m p fn v. MonadChoose m => GenOptions p fn v -> GenT m (Literal p fn v)
genUninterpretedLiteral opts = genLiteral' (genUninterpretedAtom opts)

genEqualityLiteral :: forall m p fn v. MonadChoose m => GenOptions p fn v -> GenT m (Literal p fn v)
genEqualityLiteral opts = genLiteral' (genEqualityAtom opts)


genUninterpretedAtom :: forall m p fn v. MonadChoose m => GenOptions p fn v -> GenT m (Atom p fn v)
genUninterpretedAtom opts@GenOptions{sig} = do
  let predicateSymbols = Set.toList (sigPredicateSymbols sig)
  p <- choose predicateSymbols
  args <- local (over depth (+1)) $ replicateM (arity p) (genTerm opts)
  return (Uninterpreted (symbol p) args)


genEqualityAtom :: forall m p fn v. MonadChoose m => GenOptions p fn v -> GenT m (Atom p fn v)
genEqualityAtom opts = do
  t1 <- local (over depth (+1)) (genTerm opts)
  t2 <- local (over depth (+1)) (genTerm opts)
  return (Equality t1 t2)


genSubstitution
  :: forall m p fn v. (MonadChoose m, Ord v)
  => GenOptions p fn v   -- ^ options control the generation of terms in the range
  -> [v]   -- ^ domain of the generated substitution (will be identity on all other values)
  -> GenT m (Substitution fn v)
genSubstitution opts domain = do
  pairs <- traverse (\v -> fmap (v,) (genTerm opts)) domain
  return $ Substitution (Map.fromList pairs)


assertM :: Applicative m => Bool -> m ()
assertM b = assert b (pure ())


showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty defaultLayoutOptions . align . pretty

printPretty :: Pretty a => a -> IO ()
printPretty x = putDoc (pretty x) >> putStrLn ""
