{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FormulaSpec (spec) where

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Logic.Propositional.Formula
import Exagen


instance Eq a => EqProp (Formula a) where
  (=-=) = eq


spec :: Spec
spec = do
  modifyMaxSuccess (const 500) $ do
    describe "criteria" $ do
      it "different ways to count equivalences gives the same result" $
        property $ \(fm :: Formula Int) -> countSubformula isIff fm == numIff fm

    describe "Formula instances" $ do
      modifyMaxSuccess (const 10) $ do
        let trigger = undefined :: Formula (Int, Int, Int)
        testBatch $ functor trigger
        testBatch $ applicative trigger
        testBatch $ monad trigger
