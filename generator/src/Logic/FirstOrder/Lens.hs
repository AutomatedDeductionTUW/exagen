{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.FirstOrder.Lens where

-- base
import Control.Applicative

-- lens
import Control.Lens

-- exagen
import Logic.FirstOrder.Types



-- tvariables :: Applicative f => (v -> f v') -> Term fn v -> f (Term fn v')
tvariables :: Traversal (Term fn v) (Term fn v') v v'
tvariables handler (Var v) = fmap Var (handler v)
tvariables handler (App fn ts) = App fn <$> (traverse (tvariables handler) ts)



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
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Atom p fn v -> f (Atom p fn' v')
  terms handler (Equality t1 t2) = liftA2 Equality (handler t1) (handler t2)
  terms handler (Uninterpreted p ts) = fmap (Uninterpreted p) (traverse handler ts)

instance HasTerms fn v fn' v' (Literal p fn v) (Literal p fn' v') where
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Literal p fn v -> f (Literal p fn' v')
  terms handler (Literal pos a) = Literal pos <$> (a & terms %%~ handler)

instance HasTerms fn v fn' v' (Clause p fn v) (Clause p fn' v') where
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Clause p fn v -> f (Clause p fn' v')
  terms handler (Clause ls) = Clause <$> (ls & each . terms %%~ handler)

-- instance HasTerms fn v fn' v' a a
--          => HasTerms fn v fn' v' [a] [a] where
--   terms = error "TODO"
