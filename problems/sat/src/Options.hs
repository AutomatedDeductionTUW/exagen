{-# LANGUAGE OverloadedLabels #-}

module Options
  ( Options(..)
  , SATOptions(..)
  , parseOptions
  ) where

-- optparse-applicative
import Options.Applicative



data Options
  = GenSAT SATOptions


data SATOptions = SATOptions
  { numExams :: Int
  , outputDir :: Maybe String
  }
  deriving Show


options :: Parser Options
options =
  hsubparser
  . mconcat
  . fmap (uncurry command) $
  [ ("sat", GenSAT <$> satOptionsInfo)
  ]


optionsInfo :: ParserInfo Options
optionsInfo =
  info (options <**> helper)
  (fullDesc
   <> progDesc "Generate exam problems"
   <> header "exagen")


satOptions :: Parser SATOptions
satOptions =
  pure SATOptions
  <*> optNumExams
  <*> optional optOutputDir
  where
    optNumExams = option auto $
      short 'n'
      <> value 10
      <> showDefault
      <> help "How many exams to generate"

    optOutputDir = strOption $
      long "output"
      <> metavar "DIR"
      <> help "Base directory for output. Exams will be written to folder $DIR/exam-$N/"


satOptionsInfo :: ParserInfo SATOptions
satOptionsInfo =
  info satOptions (progDesc "Generate exam problem for the SAT part")


parseOptions :: IO Options
parseOptions = do
  customExecParser (prefs showHelpOnError) optionsInfo
