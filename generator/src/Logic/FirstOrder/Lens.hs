{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Logic.FirstOrder.Lens where

-- base
import Control.Applicative

-- lens
import Control.Lens

-- exagen
import Logic.FirstOrder.Types



tsubterms :: Term fn v -> [Term fn v]
tsubterms t = t : tproperSubterms t

tproperSubterms :: Term fn v -> [Term fn v]
tproperSubterms (Var _) = []
tproperSubterms (App _ ts) = concatMap tsubterms ts


-- tvariables :: Applicative f => (v -> f v') -> Term fn v -> f (Term fn v')
tvariables :: Traversal (Term fn v) (Term fn v') v v'
tvariables = traverse
-- tvariables handler (Var v) = Var <$> handler v
-- tvariables handler (App fn ts) = App fn <$> (traverse (tvariables handler) ts)


tfunctionSymbols :: Traversal (Term fn v) (Term fn' v) fn fn'
tfunctionSymbols _ (Var v) = pure (Var v)
tfunctionSymbols handler (App fn ts) = App <$> handler fn <*> (traverse (tfunctionSymbols handler) ts)


variables :: HasTerms fn v fn v' a a' => Traversal a a' v v'
variables = terms . tvariables


functionSymbols :: HasTerms fn v fn' v a a' => Traversal a a' fn fn'
functionSymbols = terms . tfunctionSymbols


subterms :: HasTerms' fn v a => Fold a (Term fn v)
subterms = terms . to tsubterms . folded


class HasTerms fn v fn' v' a a'
      | a -> fn
      , a -> v
      , a' -> fn'
      , a' -> v'
      , a fn' v' -> a'
      , a' fn v -> a
      where
  terms :: Traversal a a' (Term fn v) (Term fn' v')

type HasTerms' fn v a = HasTerms fn v fn v a a

instance HasTerms fn v fn' v' (Term fn v) (Term fn' v') where
  terms = id

instance HasTerms fn v fn' v' (Atom p fn v) (Atom p fn' v') where
  terms handler (Equality t1 t2) = liftA2 Equality (handler t1) (handler t2)
  terms handler (Uninterpreted p ts) = fmap (Uninterpreted p) (traverse handler ts)

instance HasTerms fn v fn' v' (Literal p fn v) (Literal p fn' v') where
  terms handler (Literal pos a) = Literal pos <$> terms handler a

instance HasTerms fn v fn' v' (Clause p fn v) (Clause p fn' v') where
  terms handler (Clause ls) = Clause <$> (traverse . terms) handler ls

instance HasTerms fn v fn' v' (Inference p fn v) (Inference p fn' v') where
  terms handler (Inference ps c) =
    Inference <$> (traverse . terms) handler ps <*> terms handler c

{-
-- This needs the UndecidableInstances extension because it does not satisfy the coverage condition.
-- instance HasTerms fn v fn' v' a a => HasTerms fn v fn' v' [a] [a] where
instance (HasTerms fn v fn' v' a a, Traversable t) => HasTerms fn v fn' v' (t a) (t a) where
  terms = traverse . terms
-}


class HasAtoms p fn v p' fn' v' a a'
      | a -> p
      , a -> fn
      , a -> v
      , a' -> p'
      , a' -> fn'
      , a' -> v'
      , a p' fn' v' -> a'
      , a' p fn v -> a
      where
  atoms :: Traversal a a' (Atom p fn v) (Atom p' fn' v')

type HasAtoms' p fn v a = HasAtoms p fn v p fn v a a

instance HasAtoms p fn v p' fn' v' (Atom p fn v) (Atom p' fn' v') where
  atoms = id

instance HasAtoms p fn v p' fn' v' (Literal p fn v) (Literal p' fn' v') where
  atoms handler (Literal pos a) = Literal pos <$> handler a

instance HasAtoms p fn v p' fn' v' (Clause p fn v) (Clause p' fn' v') where
  atoms handler (Clause ls) = Clause <$> (traverse . atoms) handler ls

instance HasAtoms p fn v p' fn' v' (Inference p fn v) (Inference p' fn' v') where
  atoms handler (Inference ps c) =
    Inference <$> (traverse . atoms) handler ps <*> atoms handler c


atom_predicateSymbols :: Traversal (Atom p fn v) (Atom p' fn v) p p'
atom_predicateSymbols _ (Equality t1 t2) = pure (Equality t1 t2)
atom_predicateSymbols handler (Uninterpreted p ts) = Uninterpreted <$> handler p <*> pure ts

predicateSymbols :: HasAtoms p fn v p' fn v a a' => Traversal a a' p p'
predicateSymbols = atoms . atom_predicateSymbols


-- class HasPredicateSymbols p p' a a'
--       | a -> p
--       , a' -> p'
--       , a p' -> a'
--       , a' p -> a
--       where
--   predicateSymbols :: Traversal a a' p p'

-- instance HasPredicateSymbols p p' (Atom p fn v) (Atom p' fn v) where
--   predicateSymbols handler (Equality t1 t2) = pure (Equality t1 t2)
--   predicateSymbols handler (Uninterpreted p ts) = pure (Equality t1 t2)
