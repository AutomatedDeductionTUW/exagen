module Options
  ( Options(..)
  , Command(..)
  , SATOptions(..)
  , parseOptions
  ) where

-- optparse-applicative
import Options.Applicative



-- | Global options
data Options = Options
  { optNumExams :: Int
  , optOutputDir :: Maybe FilePath
  , optSeed :: Maybe Int
  , optCommand :: Command
  }
  deriving Show


-- | Subcommand to execute
data Command
  = GenSAT SATOptions
  deriving Show


-- | Options for subcommand "sat"
data SATOptions = SATOptions
  deriving Show


options :: Parser Options
options =
  pure Options
  <*> numExams
  <*> optional outputDir
  <*> optional seed
  <*> cmd
  where
    numExams = option auto $
      short 'n'
      <> value 10
      <> showDefault
      <> help "How many exams to generate"

    outputDir = strOption $
      short 'o'
      <> long "output"
      <> metavar "DIR"
      <> help "Base directory for output. Exams will be written to folder $DIR/exam-$N/"

    seed = option auto $
      short 's'
      <> long "seed"
      <> metavar "SEED"
      <> help "Set initial random generator seed."

    cmd = hsubparser . mconcat . fmap (uncurry command) $
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


satOptionsInfo :: ParserInfo SATOptions
satOptionsInfo =
  info satOptions (progDesc "Generate exam problem for the SAT part")


parseOptions :: IO Options
parseOptions = do
  customExecParser (prefs showHelpOnError) optionsInfo
