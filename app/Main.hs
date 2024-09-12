module Main (main) where

import Request
import Options.Applicative
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as B
import Data.Aeson
import Network.HTTP.Req hiding (header)

main :: IO ()
main = do
  (command, apikey) <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  response <- runReq defaultHttpConfig $ request (requestBody command) apikey
  print (responseBody response :: Value)

-- | Parse a command
parseCommand :: Parser Command
parseCommand =
  strArgument
    (metavar "COMMAND" 
    <> help "The command to be corrected")

-- | Parse an OpenAI api key (only for development)
parseAPIKey :: Parser ByteString
parseAPIKey =
  B.pack <$>
    strArgument
      (metavar "API_KEY"
      <> help "OpenAI api key (temporary)")

-- | Parse a command and an OpenAI api key (only for development)
parseCommandAndAPIKey :: Parser (Command, ByteString)
parseCommandAndAPIKey =
  liftA2 (,) parseCommand parseAPIKey

-- | Description of the program, with options
opts :: ParserInfo (Command , ByteString)
opts =
  info
    (parseCommandAndAPIKey <**> helper) -- ^Change to only parse command (development)
    (fullDesc 
    <> progDesc "The Fix is a command line program which corrects the previous command." 
    <> header "The Fix - correct console commands")
