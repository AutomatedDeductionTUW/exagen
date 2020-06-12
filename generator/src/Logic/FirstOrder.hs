{-# LANGUAGE ConstraintKinds #-}
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
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens

-- exagen
import Logic.FirstOrder.Lens
import Logic.FirstOrder.Types


complementary :: Literal p fn v -> Literal p fn v
complementary (Literal pos a) = Literal (not pos) a


-- | Returns true if the structure does not contain any variables, i.e., is ground.
isGround :: HasTerms' fn v a => a -> Bool
isGround = nullOf variables


term_applySubstitution :: Ord v => Substitution fn v -> Term fn v -> Term fn v
term_applySubstitution (Substitution s) t = t >>= (\v -> fromMaybe (Var v) (Map.lookup v s))


applySubstitution :: (HasTerms' fn v a, Ord v) => Substitution fn v -> a -> a
applySubstitution theta = over terms (term_applySubstitution theta)


-- | Set of function symbols (including constants) that appear in the structure
functionSymbolsOf :: (HasTerms' fn v a, Ord fn) => a -> Set fn
functionSymbolsOf = Set.fromList . toListOf functionSymbols


-- | Set of variables that appear in the structure
variablesOf :: (HasTerms' fn v a, Ord v) => a -> Set v
variablesOf = Set.fromList . toListOf variables


-- | All (top-level) terms that appear in the structure
termsOf :: HasTerms' fn v a => a -> [Term fn v]
termsOf = toListOf terms


-- | All subterms of terms that appear in the structure
subtermsOf :: HasTerms' fn v a => a -> [Term fn v]
subtermsOf = toListOf subterms



type HasSignature p fn v a = (HasAtoms' p fn v a, HasTerms' fn v a, Ord fn, Ord p)

actualSignature :: HasSignature p fn v a => a -> Signature p fn
actualSignature x = Signature
  { sigFunctionSymbols = Set.fromList (x ^.. actualFunctionSymbols)
  , sigPredicateSymbols = Set.fromList (x ^.. actualPredicateSymbols)
  }

actualFunctionSymbols :: HasTerms' fn v a => Fold a (Symbol fn)
actualFunctionSymbols = subterms . to fnSymbol . _Just
  where
    fnSymbol :: Term fn v -> Maybe (Symbol fn)
    fnSymbol (Var _) = Nothing
    fnSymbol (App fn args) = Just (Symbol fn (length args))

actualPredicateSymbols :: HasAtoms' p fn v a => Fold a (Symbol p)
actualPredicateSymbols = atoms . to predSymbol . _Just
  where
    predSymbol :: Atom p fn v -> Maybe (Symbol p)
    predSymbol (Equality _ _) = Nothing
    predSymbol (Uninterpreted p args) = Just (Symbol p (length args))
