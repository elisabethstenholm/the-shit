-- | This module contains the 'correct' function, which suggests and
--   runs a corrected command
module Correct
  ( correct
  ) where

import           Request

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as ByteString
import           Data.Maybe
import           Data.Text             (Text)
import           Network.HTTP.Req      hiding (header)
import           System.Console.ANSI
import           System.Environment
import           System.IO

correct :: Text -> String -> IO ()
correct cmd apiKeyVarName = do
  maybeApiKey <- lookupEnv apiKeyVarName
  case maybeApiKey of
    Nothing ->
      putStrLn $ "Error: No environment variable " ++ apiKeyVarName ++ "."
    Just apiKey -> do
      response <-
        runReq defaultHttpConfig $
        request (requestBody cmd) (ByteString.pack apiKey)
      case listToMaybe $ suggestions (responseBody response) of
        Nothing -> putStrLn "No suggestions."
        Just suggestion -> do
          catch (simpleTui suggestion) handleUserInterrupt
          putStr suggestion

-- | Print suggested command and keys for accepting or rejecting the suggestion
--   Only for single suggestion
simpleTui :: String -> IO ()
simpleTui suggestion = do
  hSetEcho stdin False
  stderrSupportsANSI <- hNowSupportsANSI stderr
  if stderrSupportsANSI
    then do
      hPutStr stderr $ suggestion ++ " ["
      hSetSGR stderr [SetColor Foreground Vivid Green]
      hPutStr stderr "Enter"
      hSetSGR stderr [Reset]
      hPutStr stderr "/"
      hSetSGR stderr [SetColor Foreground Vivid Red]
      hPutStr stderr "Ctrl+C"
      hSetSGR stderr [Reset]
      hPutStr stderr "]"
    else hPutStr stderr $ suggestion ++ " [Ctrl+C/Enter]"
  hFlush stderr
  _ <- getLine
  hPutStr stderr "\n"
  hSetEcho stdin True

-- | Exit program on UserInterrupt
handleUserInterrupt :: AsyncException -> IO ()
handleUserInterrupt UserInterrupt = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  when stdoutSupportsANSI (setSGR [SetColor Foreground Vivid Red])
  putStrLn "\nAborted."
  throwIO UserInterrupt
handleUserInterrupt e = throwIO e
