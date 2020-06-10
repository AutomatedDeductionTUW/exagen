{-# LANGUAGE ScopedTypeVariables #-}

module Logic.FirstOrder
  ( module Logic.FirstOrder
  , module Logic.FirstOrder.Types
  , module Logic.FirstOrder.Lens
  ) where

-- base
import Data.Maybe

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- lens
import Control.Lens

-- exagen
import Logic.FirstOrder.Lens
import Logic.FirstOrder.Types


complementary :: Literal p fn v -> Literal p fn v
complementary (Literal pos a) = Literal (not pos) a



functionSymbols :: (HasTerms' fn v a, Ord fn) => a -> Set fn
functionSymbols = Set.fromList . mapMaybe getFn . subtermsOf
  where
    getFn (Var _) = Nothing
    getFn (App fn _) = Just fn


variables :: (HasTerms' fn v a, Ord v) => a -> Set v
variables = Set.fromList . mapMaybe getVar . subtermsOf
  where
    getVar (Var v) = Just v
    getVar (App _ _) = Nothing


-- | All (top-level) terms that appear in the structure
termsOf :: HasTerms' fn v a => a -> [Term fn v]
termsOf a = a ^.. terms

-- | All subterms of terms that appear in the structure
subtermsOf :: HasTerms' fn v a => a -> [Term fn v]
subtermsOf a = concatMap tsubterms (termsOf a)

-- TODO: subterms should be a Fold, variables and functionSymbols could be too


tsubterms :: Term fn v -> [Term fn v]
tsubterms t = t : tproperSubterms t

tproperSubterms :: Term fn v -> [Term fn v]
tproperSubterms (Var _) = []
tproperSubterms (App _ ts) = concatMap tsubterms ts
