module Main
  ( main
  ) where

import           Alias                           (alias)
import           Correct                         (correct)
import           Shell                           (toShell)

import           Options.Applicative             (CommandFields, Mod, Parser,
                                                  ParserInfo, auto, command,
                                                  customExecParser, help,
                                                  helper, info, long, metavar,
                                                  option, prefs, short,
                                                  showDefault, showHelpOnEmpty,
                                                  showHelpOnError, strArgument,
                                                  strOption, subparser, value,
                                                  (<**>))
import           System.Posix.Process.ByteString (getParentProcessID)
import           System.Process                  (readProcess)

main :: IO ()
main = do
  ppid <- getParentProcessID
  shell <-
    (toShell . filter (/= '\n')) <$>
    readProcess "ps" ["-p", show ppid, "-o", "comm="] ""
  mode <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  case mode of
    CorrectMode cmd apiKeyVarName temp -> correct cmd apiKeyVarName temp shell
    AliasMode apiKeyVarName aliasName temp ->
      alias apiKeyVarName aliasName temp shell

-- | Program mode
data Mode
  = CorrectMode String String Double -- ^Correct console command. Arguments: command, api key variable, temperature
  | AliasMode String String Double -- ^Construct alias. Arguments: api key variable, alias, temperature
  deriving (Eq, Ord, Show)

parseCorrectMode :: Mod CommandFields Mode
parseCorrectMode =
  command
    "correct"
    (info
       ((CorrectMode <$> parseCommand <*> parseApiKey <*> parseTemperature) <**>
        helper)
       mempty)

parseAliasMode :: Mod CommandFields Mode
parseAliasMode =
  command
    "alias"
    (info
       ((AliasMode <$> parseApiKey <*> parseAliasName <*> parseTemperature) <**>
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
