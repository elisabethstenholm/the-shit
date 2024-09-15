module Main
  ( main
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as ByteString
import           Data.Maybe
import           Data.Text             (Text)
import           Network.HTTP.Req      hiding (header)
import           Options.Applicative   hiding (command)
import           Request
import           System.Console.ANSI
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

main :: IO ()
main = do
  args <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  let varName = apiKeyVarName args
  maybeApiKey <- lookupEnv $ varName
  case maybeApiKey of
    Nothing -> putStrLn $ "Error: No environment variable " ++ varName ++ "."
    Just apiKey -> do
      response <-
        runReq defaultHttpConfig $
        request (requestBody (command args)) (ByteString.pack apiKey)
      case listToMaybe $ suggestions (responseBody response) of
        Nothing -> putStrLn "No suggestions."
        Just suggestion -> do
          catch (simpleTui suggestion) handleUserInterrupt
          _ <- try (callCommand suggestion) :: IO (Either IOError ())
          return ()

-- | Print suggested command and keys for accepting or rejecting the suggestion
--   Only for single suggestion
simpleTui :: String -> IO ()
simpleTui suggestion = do
  hSetEcho stdin False
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      putStr $ suggestion ++ " ["
      setSGR [SetColor Foreground Vivid Green]
      putStr "Enter"
      setSGR [Reset]
      putStr "/"
      setSGR [SetColor Foreground Vivid Red]
      putStr "Ctrl+C"
      setSGR [Reset]
      putStr "]"
    else putStr $ suggestion ++ " [Ctrl+C/Enter]"
  hFlush stdout
  _ <- getLine
  putStr "\n"
  hSetEcho stdin True

handleUserInterrupt :: AsyncException -> IO ()
handleUserInterrupt UserInterrupt = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  when stdoutSupportsANSI (setSGR [SetColor Foreground Vivid Red])
  putStrLn "\nAborted."
  exitSuccess
handleUserInterrupt e = throwIO e

-- | All arguments to the program
data Arguments =
  Arguments
    { command       :: Text -- ^The command to be corrected
    , apiKeyVarName :: String -- ^Name of environment variable containing OpenAI api key
    }

-- | Parse arguments to the program
parseArguments :: Parser Arguments
parseArguments =
  Arguments <$>
  strArgument (metavar "COMMAND" <> help "The command to be corrected") <*>
  strOption
    (long "var-name" <>
     short 'n' <>
     help "Name of environment variable containing OpenAI api key" <>
     showDefault <> value "OPENAI_API_KEY" <> metavar "NAME")

-- | Description of the program, with options
opts :: ParserInfo Arguments
opts =
  info
    (parseArguments <**> helper)
    (fullDesc <>
     progDesc
       "The Fix is a command line program which corrects the previous command." <>
     header "The Fix - correct console commands")
