{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.SmtLib
  ( ToSmtLib(..)
  , Sort
  , Statement(..)
  , formatSmtLib
  , declareSignature
  , assertClause
  , assertNotClause
  ) where

-- base
import Data.Foldable

-- exagen
import Data.SExpr
import Logic.FirstOrder


class ToSmtLib a where
  toSmtLib :: a -> Expr String


instance ToSmtLib String where
  toSmtLib = Value

instance (ToSmtLib fn, ToSmtLib v) => ToSmtLib (Term fn v) where
  toSmtLib (Var v) = toSmtLib v
  toSmtLib (App c []) = toSmtLib c
  toSmtLib (App f ts) = SExpr (toSmtLib f : map toSmtLib ts)

instance (ToSmtLib p, ToSmtLib fn, ToSmtLib v) => ToSmtLib (Atom p fn v) where
  toSmtLib (Equality t1 t2) = SExpr [Value "=", toSmtLib t1, toSmtLib t2]
  toSmtLib (Uninterpreted p ts) = SExpr (toSmtLib p : map toSmtLib ts)

instance (ToSmtLib p, ToSmtLib fn, ToSmtLib v) => ToSmtLib (Literal p fn v) where
  toSmtLib (Literal True a) = toSmtLib a
  toSmtLib (Literal False a) = SExpr [Value "not", toSmtLib a]

instance (ToSmtLib p, ToSmtLib fn, ToSmtLib v) => ToSmtLib (Clause p fn v) where
  toSmtLib (Clause ls) = SExpr (Value "or" : map toSmtLib ls)


type Sort = String

data Statement a
  = DeclareSort Sort
  | DeclareFun a [Sort] Sort
  | DeclareConst a Sort
  | Assert a
  | AssertNot a
  | CheckSat
  deriving (Eq, Ord, Show)

instance ToSmtLib (Statement (Expr String)) where

  toSmtLib (DeclareSort sort) =
    SExpr [Value "declare-sort", Value sort, Value "0"]

  toSmtLib (DeclareFun fn inputSorts resultSort) =
    SExpr [Value "declare-fun", fn, SExpr (map Value inputSorts), Value resultSort]

  toSmtLib (DeclareConst fn sort) =
    SExpr [Value "declare-const", fn, Value sort]

  toSmtLib (Assert x) =
    SExpr [Value "assert", x]

  toSmtLib (AssertNot x) =
    SExpr [Value "assert", SExpr [Value "not", x]]

  toSmtLib CheckSat =
    SExpr [Value "check-sat"]


formatSmtLib :: [Statement (Expr String)] -> String
formatSmtLib = unlines . map (formatExpr id . toSmtLib)


declareSignature :: (ToSmtLib p, ToSmtLib fn) => Sort -> Signature p fn -> [Statement (Expr String)]
declareSignature sort sig =
  [ DeclareSort sort ]
  ++ [ declFn fn | fn <- toList (sigFunctionSymbols sig) ]
  ++ [ declPred p | p <- toList (sigPredicateSymbols sig) ]

  where
    declFn (Symbol fn 0) = DeclareConst (toSmtLib fn) sort
    declFn (Symbol fn n) = DeclareFun (toSmtLib fn) (replicate n sort) sort

    declPred (Symbol p n) = DeclareFun (toSmtLib p) (replicate n sort) "Bool"


-- (forall ((x Int)(y Int)) (or …))
universalClosure
  :: (HasTerms' fn v a, ToSmtLib a, ToSmtLib v, Ord v)
  => Sort -> a -> Expr String
universalClosure varSort cl =
  case bindings of
    [] -> toSmtLib cl
    _ -> SExpr [Value "forall", SExpr bindings, toSmtLib cl]
  where
    bindings = [ SExpr [ toSmtLib v, Value varSort ] | v <- toList (variablesOf cl) ]


assertClause
  :: (ToSmtLib p, ToSmtLib fn, ToSmtLib v, Ord v)
  => Sort -> Clause p fn v -> Statement (Expr String)
assertClause varSort cl = Assert (universalClosure varSort cl)


assertNotClause
  :: (ToSmtLib p, ToSmtLib fn, ToSmtLib v, Ord v)
  => Sort -> Clause p fn v -> Statement (Expr String)
assertNotClause varSort cl = AssertNot (universalClosure varSort cl)
