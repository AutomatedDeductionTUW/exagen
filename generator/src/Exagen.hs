{-# LANGUAGE NamedFieldPuns #-}

module Exagen where

-- base
import Control.Monad
import System.IO

-- directory
import System.Directory

-- random
import System.Random

-- exagen
import Options
import qualified Problems.Redundancy
import qualified Problems.SAT
import qualified Problems.SMT


main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts@Options{optOutputDir,optSeed} <- parseOptions

  case optOutputDir of
    Nothing -> pure ()
    Just outputDir -> do
      outputDirExists <- doesDirectoryExist outputDir
      unless outputDirExists $
        fail ("The given output directory does not exist: " <> show outputDir)

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
