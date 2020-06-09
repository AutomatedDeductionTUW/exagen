{-# LANGUAGE ScopedTypeVariables #-}

module Text.Show.Latex where

-- base
import Data.List (intercalate)


class ShowLatex a where
  -- showsPrecLatex :: Int -> a -> ShowS
  showLatex :: a -> String

  showListLatex :: [a] -> ShowS
  showListLatex xs = (<> intercalate " , " (map showLatex xs))


instance ShowLatex Char where
  showLatex = pure
  showListLatex xs = (++xs)


instance ShowLatex a => ShowLatex [a] where
  showLatex xs = showListLatex xs ""
