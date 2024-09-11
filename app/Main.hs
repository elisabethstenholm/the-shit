module Main (main) where

import Lib
import Data.Aeson
import Options.Applicative

main :: IO ()
main = do
  (command, apikey) <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  print $ encode $ requestBody command

-- | Parse a command
parseCommand :: Parser Command
parseCommand =
  strArgument
    (metavar "COMMAND" 
    <> help "The command to be corrected")

-- | Parse an OpenAI api key (only for development)
parseAPIKey :: Parser String
parseAPIKey =
  strArgument
    (metavar "API_KEY"
    <> help "OpenAI api key (temporary)")

-- | Parse a command and an OpenAI api key (only for development)
parseCommandAndAPIKey :: Parser (Command, String)
parseCommandAndAPIKey =
  liftA2 (,) parseCommand parseAPIKey

-- | Description of the program, with options
opts :: ParserInfo (Command , String)
opts =
  info
    (parseCommandAndAPIKey <**> helper) -- ^Change to only parse command (development)
    (fullDesc 
    <> progDesc "The Fix is a command line program which corrects the previous command." 
    <> header "The Fix - correct console commands")
