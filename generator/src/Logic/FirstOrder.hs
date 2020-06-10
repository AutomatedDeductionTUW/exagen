{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.FirstOrder where

-- base
import Control.Applicative
import Control.Exception (assert)
import Control.Monad (ap)
import Control.Monad.Fail
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Void
import Debug.Trace

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

-- exagen
import Text.Show.Latex





--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

data Signature p fn = Signature
  { sigFunctionSymbols :: Set (Symbol fn)
  , sigPredicateSymbols :: Set (Symbol p)
  }

data Symbol a = Symbol
  { symbol :: !a
  , arity :: !Int
  }
  deriving (Eq, Ord, Show, Functor)


--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

data Inference p fn v = Inference
  { premises :: [Clause p fn v]
  , conclusion :: Clause p fn v
  }
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- First-Order Terms, Atoms, Literals, Clauses
--------------------------------------------------------------------------------

newtype Clause p fn v = Clause
  { literals :: [Literal p fn v]
  }
  deriving (Eq, Ord, Show, Functor, Foldable)

data Literal p fn v = Literal
  { isPositive :: Bool
  , atom :: Atom p fn v
  }
  deriving (Eq, Ord, Show, Functor, Foldable)

data Atom p fn v
  = Equality (Term fn v) (Term fn v)
  | Uninterpreted p [Term fn v]
  deriving (Eq, Ord, Show, Functor, Foldable)

data Term fn v
  = Var v
  | App fn [Term fn v]
  deriving (Eq, Ord, Show, Functor, Foldable)


--------------------------------------------------------------------------------
-- Term is a Monad (via substitution of variables by terms)
--------------------------------------------------------------------------------

instance Applicative (Term fn) where
  pure = Var
  (<*>) = ap

instance Monad (Term fn) where
  t >>= f = joinTerm (fmap f t)

joinTerm :: Term fn (Term fn v) -> Term fn v
joinTerm (Var t) = t
joinTerm (App fn ts) = App fn (joinTerm <$> ts)


--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Clause p fn v) where
  pretty (Clause []) = "⚡️"
  pretty (Clause (l:ls)) = pretty l <+> nest 4 (fillSep (map (\x -> "\\/" <+> pretty x) ls))

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Literal p fn v) where
  pretty (Literal True (Equality t1 t2)) = pretty t1 <+> "=" <+> pretty t2
  pretty (Literal False (Equality t1 t2)) = pretty t1 <+> "≠" <+> pretty t2
  pretty (Literal True a) = pretty a
  pretty (Literal False a) = "¬" <> pretty a

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Atom p fn v) where
  pretty (Equality t1 t2) = pretty t1 <+> "=" <+> pretty t2
  pretty (Uninterpreted p args) = pretty p <> align (encloseSep lparen rparen comma (map pretty args))

instance (Pretty fn, Pretty v) => Pretty (Term fn v) where
  pretty (Var v) = pretty v
  pretty (App c []) = pretty c
  pretty (App fn args) = pretty fn <> align (encloseSep lparen rparen comma (map pretty args))

instance (Pretty p, Pretty fn, Pretty v) => Pretty (Inference p fn v) where
  pretty Inference{premises,conclusion} =
    vsep (pretty <$> premises) <> hardline
    <> pretty (replicate 50 '-') <> hardline
    <> pretty conclusion


--------------------------------------------------------------------------------
-- Latex output
--------------------------------------------------------------------------------

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Clause p fn v) where
  showLatex (Clause []) = " \\Box "
  showLatex (Clause ls) = intercalate " \\lor " (map showLatex ls)


instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Literal p fn v) where
  showLatex (Literal True (Equality t1 t2)) = showLatex t1 <> " = " <> showLatex t2
  showLatex (Literal False (Equality t1 t2)) = showLatex t1 <> " \\neq " <> showLatex t2
  showLatex (Literal True a) = showLatex a
  showLatex (Literal False a) = " \\lnot " <> showLatex a

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Atom p fn v) where
  showLatex (Equality t1 t2) = showLatex t1 <> " = " <> showLatex t2
  showLatex (Uninterpreted p args) = showLatex p <> " ( " <> intercalate " , " (map showLatex args) <> " )"

instance (ShowLatex fn, ShowLatex v) => ShowLatex (Term fn v) where
  showLatex (Var v) = showLatex v
  showLatex (App c []) = showLatex c
  showLatex (App fn args) = showLatex fn <> " ( " <> intercalate " , " (map showLatex args) <> " )"

instance (ShowLatex p, ShowLatex fn, ShowLatex v) => ShowLatex (Inference p fn v) where
  showLatex Inference{premises,conclusion} =
    "\\prftree" <> concat (wrap . showLatex <$> premises) <> wrap (showLatex conclusion)
    where
      wrap s = '{' : s ++ "}"
