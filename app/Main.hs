module Main
  ( main
  ) where

import           Alias               (alias)
import           Correct             (correct)

import           Options.Applicative (CommandFields, Mod, Parser, ParserInfo,
                                      command, customExecParser, help, helper,
                                      info, long, metavar, prefs, short,
                                      showDefault, showHelpOnEmpty,
                                      showHelpOnError, strArgument, strOption,
                                      subparser, value, (<**>))

main :: IO ()
main = do
  mode <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  case mode of
    CorrectMode cmd apiKeyVarName     -> correct cmd apiKeyVarName
    AliasMode apiKeyVarName aliasName -> alias apiKeyVarName aliasName

-- | Program mode
data Mode
  = CorrectMode String String -- ^Correct console command. Arguments: command, api key variable
  | AliasMode String String -- ^Construct alias. Arguments: api key variable, alias
  deriving (Eq, Ord, Show)

parseCorrectMode :: Mod CommandFields Mode
parseCorrectMode =
  command
    "correct"
    (info ((CorrectMode <$> parseCommand <*> parseApiKey) <**> helper) mempty)

parseAliasMode :: Mod CommandFields Mode
parseAliasMode =
  command
    "alias"
    (info ((AliasMode <$> parseApiKey <*> parseAliasName) <**> helper) mempty)

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

parseAliasName :: Parser String
parseAliasName =
  strOption
    (long "alias-name" <>
     short 'a' <>
     help "Alias name" <> showDefault <> value "shit" <> metavar "ALIAS")

opts :: ParserInfo Mode
opts = info (subparser (parseCorrectMode <> parseAliasMode) <**> helper) mempty
