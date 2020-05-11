{-# LANGUAGE TypeApplications #-}

module Exagen where

-- base
import Data.Foldable

-- prettyprinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

-- QuickCheck
import Test.QuickCheck

-- exagen
import Logic.Propositional.Formula


main :: IO ()
main = do
  fms <- sample' (resize 10 $ arbitrary @(Formula Prop))
  traverse_ (putStrLn . showPretty) fms





showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty layoutOptions . align . pretty
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }
