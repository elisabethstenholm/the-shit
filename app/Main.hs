module Main (main) where

import Request
import Options.Applicative
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Data.Aeson
import Network.HTTP.Req hiding (header)
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
  (command, prevOutput, apikey) <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  response <- runReq defaultHttpConfig $ request (requestBody command (Text.pack prevOutput)) (ByteString.pack apikey)
  case suggestions (responseBody response) of
    Left s -> print s
    Right ss -> tui $ read $ show $ head ss

tui :: String -> IO ()
tui suggestion = do
  hSetEcho stdin False
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      putStr $ suggestion ++ " ["
      setSGR [SetColor Foreground Dull Green]
      putStr "Enter"
      setSGR [Reset]
      putStr "/"
      setSGR [SetColor Foreground Dull Red]
      putStr "Ctrl+C"
      setSGR [Reset]
      putStr "]"
    else
      putStr $ suggestion ++ " [Ctrl+C/Enter]"
  hFlush stdout
  _ <- getLine
  return ()

-- | Parse a command
parseCommand :: Parser Command
parseCommand =
  strArgument
    (metavar "COMMAND" 
    <> help "The command to be corrected")

-- | Parse output from previous command
parseOutput :: Parser String
parseOutput =
  strArgument
    (metavar "PREV_OUTPUT"
    <> help "Output from previous command"
    <> showDefault
    <> value "")

-- | Parse an OpenAI api key (only for development)
parseAPIKey :: Parser String
parseAPIKey =
  strArgument
    (metavar "API_KEY"
    <> help "OpenAI api key (temporary)")

-- | Parse a command and an OpenAI api key (only for development)
parseAll :: Parser (Command, String, String)
parseAll =
  liftA3 (,,) parseCommand parseOutput parseAPIKey

-- | Description of the program, with options
opts :: ParserInfo (Command, String, String)
opts =
  info
    (parseAll <**> helper) -- ^Change to only parse command (development)
    (fullDesc 
    <> progDesc "The Fix is a command line program which corrects the previous command." 
    <> header "The Fix - correct console commands")
