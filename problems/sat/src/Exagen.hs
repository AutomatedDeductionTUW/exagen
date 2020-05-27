module Exagen where

-- exagen
import Options
import qualified Problems.SAT


main :: IO ()
main = do
  cmd <- parseOptions
  case cmd of
    GenSAT opts -> Problems.SAT.main opts
