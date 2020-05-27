{-# LANGUAGE NamedFieldPuns #-}

module Exagen where

-- random
import System.Random

-- exagen
import Options
import qualified Problems.SAT


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
    GenSAT cmdOpts -> Problems.SAT.main opts cmdOpts


-- Returns the seed that has been set
setSeed :: Maybe Int -> IO Int
setSeed Nothing = do
  -- If no seed has been specified, we generate a random one to use
  seed <- randomIO
  setSeed (Just seed)
setSeed (Just seed) = do
  setStdGen (mkStdGen seed)
  return seed
