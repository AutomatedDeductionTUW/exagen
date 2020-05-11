{-# LANGUAGE TypeApplications #-}

module Exagen where

-- QuickCheck
import Test.QuickCheck

-- exagen
import Logic.Propositional.Formula


main :: IO ()
main = do
  sample (arbitrary @(Formula Int))
