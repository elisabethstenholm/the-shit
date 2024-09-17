-- | This module contains the 'correct' function, which suggests and
--   runs a corrected command
module Correct
  ( correct
  ) where

import           Request
import           TUI

import           Control.Applicative
import           Control.Applicative.Logic
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as ByteString
import           Network.HTTP.Req      hiding (header)
import           System.Console.ANSI
import           System.Console.Terminfo hiding (Red, Green)
import           System.Environment
import           System.IO

correct :: String -> String -> IO ()
correct cmd apiKeyVarName = do
  -- Make request
  maybeApiKey <- lookupEnv apiKeyVarName
  apiKey <- convert maybeApiKey <|> fail ("No environment variable " ++ apiKeyVarName ++ ".")
  response <-
    runReq defaultHttpConfig $
    request cmd (ByteString.pack apiKey)
  -- Run interactive menu
  term <- setupTermFromEnv
  keypadOnCode <- convert (getCapability term keypadOn) <|> fail "Terminal does not support application keypad mode."
  hPutStr stderr keypadOnCode -- Enable application mode in terminal
  hFlush stderr
  upCode <- convert (getCapability term keyUp) <|> fail "Terminal does not support upward arrow key."
  downCode <- convert (getCapability term keyDown) <|> fail "Terminal does not support downward arrow key."
  -- suggs <- convert (suggestions $ responseBody response) <|> fail "No suggestions."
  menu <- convert (mkMaybeMenu $ suggestions $ responseBody response) <|> fail "No suggestions."
  finalMenu <- catch (hInteract stderr upCode downCode menu) handleUserInterrupt
  -- Write selected correction to stdout
  putStrLn $ selected finalMenu

-- | Print "Aborted." on UserInterrupt and then re-throw exception
handleUserInterrupt :: AsyncException -> IO (Menu a)
handleUserInterrupt UserInterrupt = do
  stderrSupportsANSI <- hNowSupportsANSI stderr
  when stderrSupportsANSI (hSetSGR stderr [SetColor Foreground Vivid Red])
  hPutStrLn stderr "Aborted."
  when stderrSupportsANSI (hSetSGR stderr [Reset] >> hShowCursor stderr)
  throwIO UserInterrupt
handleUserInterrupt e = throwIO e
