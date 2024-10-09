module Main
  ( main
  ) where

import           Alias               (AliasArgs (AliasArgs), alias)
import           Correct             (CorrectArgs (CorrectArgs), correct)

import           Options.Applicative (CommandFields, Mod, Parser, ParserInfo,
                                      auto, command, customExecParser, help,
                                      helper, info, long, metavar, option,
                                      prefs, short, showDefault,
                                      showHelpOnEmpty, showHelpOnError,
                                      strArgument, strOption, subparser, value,
                                      (<**>))

main :: IO ()
main = do
  mode <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  case mode of
    CorrectMode args -> correct args
    AliasMode args   -> alias args

-- | Program mode
data Mode
  = CorrectMode CorrectArgs -- ^Correct console command
  | AliasMode AliasArgs -- ^Construct alias
  deriving (Eq, Show)

parseCorrectMode :: Mod CommandFields Mode
parseCorrectMode =
  command
    "correct"
    (info
       ((CorrectMode <$>
         (CorrectArgs <$> parseCommand <*> parseApiKey <*> parseTemperature)) <**>
        helper)
       mempty)

parseAliasMode :: Mod CommandFields Mode
parseAliasMode =
  command
    "alias"
    (info
       ((AliasMode <$>
         (AliasArgs <$> parseApiKey <*> parseAliasName <*> parseTemperature)) <**>
        helper)
       mempty)

parseCommand :: Parser String
parseCommand =
  strArgument (metavar "COMMAND" <> help "The command to be corrected")

parseApiKey :: Parser String
parseApiKey =
  strOption
    (long "var-name" <>
     short 'n' <>
     help "Name of environment variable containing OpenAI api key" <>
     showDefault <> value "OPENAI_API_KEY" <> metavar "NAME")

parseTemperature :: Parser Double
parseTemperature =
  roundTemp <$>
  option
    auto
    (long "temperature" <>
     short 't' <>
     help
       "Temperature (higher = more random, lower = more deterministic). \
       \Real number between 0 and 2. Smaller numbers will be rounded to 0 and larger to 2." <>
     showDefault <> value 1 <> metavar "TEMP")

parseAliasName :: Parser String
parseAliasName =
  strOption
    (long "alias-name" <>
     short 'a' <>
     help "Alias name" <> showDefault <> value "shit" <> metavar "ALIAS")

opts :: ParserInfo Mode
opts = info (subparser (parseCorrectMode <> parseAliasMode) <**> helper) mempty

-- | Round temperature to closest real number between 0 and 2
roundTemp :: Double -> Double
roundTemp temp =
  if temp < 0
    then 0
    else if temp > 2
           then 2
           else temp
