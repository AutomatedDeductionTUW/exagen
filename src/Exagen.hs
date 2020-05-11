{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  let genProp = do
        str <- elements ["p", "q", "r"]
        return (P str)
  fms <- sample' (resize 3 $ genFormula genProp)
  forM_ fms $ \fm -> do
    putStrLn $ showPretty fm
    putStrLn ""



-- newtype PropVar = PV { getPropVar :: String }
--   deriving stock (Eq, Ord)
--   deriving newtype (Show, Pretty)

-- instance Arbitrary PropVar where
--   arbitrary = do
--     str <- elements ["p", "q", "r"]
--     return (PV str)



showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty layoutOptions . align . pretty
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }
