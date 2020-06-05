{-# LANGUAGE NamedFieldPuns #-}

module Exagen where

-- random
import System.Random

-- exagen
import Options
import qualified Problems.Redundancy
import qualified Problems.SAT
import qualified Problems.SMT


main :: IO ()
main = do
  opts@Options{optSeed} <- parseOptions

  actualSeed <- setSeed optSeed
  putStrLn $ "Random generator seed: " <> show actualSeed

  let opts' = opts{ optSeed = Just actualSeed }
  runCommand opts'


runCommand :: Options -> IO ()
runCommand opts =
  case optCommand opts of
    GenRed cmdOpts -> Problems.Redundancy.main opts cmdOpts
    GenSAT cmdOpts -> Problems.SAT.main opts cmdOpts
    GenSMT cmdOpts -> Problems.SMT.main opts cmdOpts


-- Returns the seed that has been set
setSeed :: Maybe Int -> IO Int
setSeed Nothing = do
  -- If no seed has been specified, we generate a random one to use
  seed <- randomIO
  setSeed (Just seed)
setSeed (Just seed) = do
  setStdGen (mkStdGen seed)
  return seed
