{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Problems.Redundancy where

-- base
import Control.Exception (assert)
import Control.Monad (ap)
import Data.Foldable
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

-- exagen
import Control.Monad.Choose
import Options (Options(..), RedOptions(..))


main :: Options -> RedOptions -> IO ()
main Options{optNumExams,optOutputDir,optSeed} RedOptions = do

  let sig = Set.fromList
        [ Symbol "f" 1
        , Symbol "g" 2
        , Symbol "h" 1
        , Symbol "c" 0
        , Symbol "d" 0
        ]
      vars = [ "x", "y", "z" ]
      opts = GenOptions{ minDepth = 2
                       , maxDepth = 3
                       , sig = sig
                       }

  ts <- replicateM optNumExams (randomTerm opts vars)
  forM_ ts printPretty

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
--
-- Question:
-- a) sound?
-- b) redundant?
--
-- IT SHOULD PROBABLY NOT BE SUBSUMPTION BUT SUBSUMPTION RESOLUTION (as in Laura's note)
-- otherwise we cannot ask them to prove soundness...
--
--
-- Also check the note Laura sent (ADuctExam20S.pdf)


data Term fn v
  = Var v
  | App fn [Term fn v]
  deriving (Eq, Ord, Show, Functor)

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


data Symbol a = Symbol
  { symbol :: !a
  , arity :: !Int
  }
  deriving (Eq, Ord, Show, Functor)

-- newtype Signature a = Signature { unSignature :: Set (Symbol a) }
type Signature a = Set (Symbol a)

-- type Signature a = Map a Int
-- constantSymbols = map fst . filter ((==0) . snd) . Map.toList $ sig
-- functionSymbols = filter ((>0) . snd) . Map.toList $ sig


randomTerm :: GenOptions fn -> [v] -> IO (Term fn v)
randomTerm opts vars = do
  let genVar = lift (choose vars)
  maybeTerm <- evalRandomListIO' $ runGen (genTerm opts genVar)
  case maybeTerm of
    Just t -> return t
    Nothing -> error "empty sample space"


genGroundTerm :: MonadChoose m => GenOptions fn -> GenT m (Term fn Void)
genGroundTerm go = genTerm go mzero

runGen :: GenT m a -> m a
runGen g = runReaderT g initialCtx
  where
    initialCtx = GenCtx
      { _depth = 0
      }

data GenOptions fn = GenOptions
  { minDepth :: Int
  , maxDepth :: Int
  , sig :: Signature fn
  }

type GenT m = ReaderT GenCtx m

data GenCtx = GenCtx
  { _depth :: Int   -- starts at 0 for the whole term, +1 each time we go into a term's arguments
  }

depth :: Lens' GenCtx Int
depth = lens _depth (\x y -> x{ _depth = y })

genTerm :: forall m fn v. MonadChoose m => GenOptions fn -> GenT m v -> GenT m (Term fn v)
genTerm GenOptions{minDepth,maxDepth,sig} genVar = term
  where
    constantSymbols = filter ((==0) . arity) . Set.toList $ sig
    functionSymbols = filter ((>0) . arity) . Set.toList $ sig

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
    var = Var <$> genVar




assertM :: Applicative m => Bool -> m ()
assertM b = assert b (pure ())


showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty defaultLayoutOptions . align . pretty

printPretty :: Pretty a => a -> IO ()
printPretty x = putDoc (pretty x) >> putStrLn ""
