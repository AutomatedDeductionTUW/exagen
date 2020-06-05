module Options
  ( Options(..)
  , Command(..)
  , SATOptions(..)
  , SMTOptions(..)
  , RedOptions(..)
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
  | GenSMT SMTOptions
  | GenRed RedOptions
  deriving Show


-- | Options for subcommand "sat"
data SATOptions = SATOptions
  deriving Show


-- | Options for subcommand "smt"
data SMTOptions = SMTOptions
  { optTemplate :: FilePath
  }
  deriving Show


-- | Options for subcommand "red"
data RedOptions = RedOptions
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
      <> action "directory"

    seed = option auto $
      short 's'
      <> long "seed"
      <> metavar "SEED"
      <> help "Set initial random generator seed."

    cmd = hsubparser . mconcat . fmap (uncurry command) $
      [ ("sat", GenSAT <$> satOptionsInfo)
      , ("smt", GenSMT <$> smtOptionsInfo)
      , ("red", GenRed <$> redOptionsInfo)
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


smtOptions :: Parser SMTOptions
smtOptions =
  pure SMTOptions
  <*> template
  where
    template = strOption $
      short 't'
      <> long "template"
      <> metavar "FILE"
      <> help "Path to the problem template file in SMT-LIB 2 format"
      <> action "file"


smtOptionsInfo :: ParserInfo SMTOptions
smtOptionsInfo =
  info smtOptions (progDesc "Generate exam problem for the SMT part")


redOptions :: Parser RedOptions
redOptions =
  pure RedOptions


redOptionsInfo :: ParserInfo RedOptions
redOptionsInfo =
  info redOptions (progDesc "Generate exam problem for the redundancy part")


parseOptions :: IO Options
parseOptions = do
  customExecParser (prefs showHelpOnError) optionsInfo
