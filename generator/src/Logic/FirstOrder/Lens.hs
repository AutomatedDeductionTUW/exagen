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
import Logic.FirstOrder



-- tvariables :: Applicative f => (v -> f v') -> Term fn v -> f (Term fn v')
tvariables :: Traversal (Term fn v) (Term fn v') v v'
tvariables handler (Var v) = fmap Var (handler v)
tvariables handler (App fn ts) = App fn <$> (traverse (tvariables handler) ts)



class HasTermsTraversal fn v fn' v' a a'
      | a -> fn
      , a -> v
      , a' -> fn'
      , a' -> v'
      , a fn' v' -> a'
      , a' fn v -> a
      where
  terms :: Traversal a a' (Term fn v) (Term fn' v')

instance HasTermsTraversal fn v fn' v' (Term fn v) (Term fn' v') where
  -- tterms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Term fn v -> f (Term fn' v')
  terms = id

instance HasTermsTraversal fn v fn' v' (Atom p fn v) (Atom p fn' v') where
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Atom p fn v -> f (Atom p fn' v')
  terms handler (Equality t1 t2) = liftA2 Equality (handler t1) (handler t2)
  terms handler (Uninterpreted p ts) = fmap (Uninterpreted p) (traverse handler ts)

instance HasTermsTraversal fn v fn' v' (Literal p fn v) (Literal p fn' v') where
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Literal p fn v -> f (Literal p fn' v')
  terms handler (Literal pos a) = Literal pos <$> (a & terms %%~ handler)

instance HasTermsTraversal fn v fn' v' (Clause p fn v) (Clause p fn' v') where
  terms :: Applicative f => (Term fn v -> f (Term fn' v')) -> Clause p fn v -> f (Clause p fn' v')
  terms handler (Clause ls) = Clause <$> (ls & each . terms %%~ handler)
