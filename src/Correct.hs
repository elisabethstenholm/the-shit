-- | This module contains the 'correct' function, which suggests and
--   runs a corrected command
module Correct
  ( correct
  ) where

import           Request                   (request, suggestions)
import           TUI                       (Menu (selected), hInteractWithMenu,
                                            mkMaybeMenu)

import           Control.Applicative       ((<|>))
import           Control.Applicative.Logic (convert)
import           Control.Exception         (AsyncException (UserInterrupt),
                                            catch, throwIO)
import           Control.Monad             (when)
import qualified Data.ByteString.Char8     as ByteString
import           Data.Maybe                (fromMaybe)
import           Network.HTTP.Req          (defaultHttpConfig, responseBody,
                                            runReq)
import           System.Console.ANSI       (Color (Red), ColorIntensity (Vivid),
                                            ConsoleLayer (Foreground),
                                            SGR (Reset, SetColor),
                                            hNowSupportsANSI, hSetSGR,
                                            hShowCursor)
import           System.Console.Terminfo   (getCapability, keyDown, keyUp,
                                            keypadOn, setupTermFromEnv)
import           System.Environment        (lookupEnv)
import           System.IO                 (Handle, hFlush, hPutStr, hPutStrLn,
                                            stderr, stdin)

correct :: String -> String -> Double -> IO ()
correct cmd apiKeyVarName temp = do
  choices <- sendRequest cmd apiKeyVarName temp
  choice <- hGetUserChoice stdin stderr choices
  putStrLn $ choice

-- | Send request to OpenAI and parse the response as a list of commands
sendRequest :: String -> String -> Double -> IO [String]
sendRequest cmd apiKeyVarName temp = do
  apiKey <-
    (lookupEnv apiKeyVarName >>= convert) <|>
    fail ("No environment variable " ++ apiKeyVarName ++ ".")
  aliases <- (lookupEnv "TS_SHELL_ALIASES" >>= (return . fromMaybe ""))
  prevCmds <- (lookupEnv "TS_HISTORY" >>= (return . fromMaybe ""))
  suggestions <$> responseBody <$>
    (runReq defaultHttpConfig $
     request cmd aliases prevCmds (ByteString.pack apiKey) temp)

-- | Interact with user and return selected command or abort on user interruption
hGetUserChoice :: Handle -> Handle -> [String] -> IO String
hGetUserChoice hdlIn hdlOut choices = do
  menu <- convert (mkMaybeMenu $ choices) <|> fail "No suggestions."
  term <- setupTermFromEnv
  keypadOnCode <-
    convert (getCapability term keypadOn) <|>
    fail "Terminal does not support application keypad mode."
  hPutStr hdlOut keypadOnCode
  hFlush hdlOut
  upCode <-
    convert (getCapability term keyUp) <|>
    fail "Terminal does not support upward arrow key."
  downCode <-
    convert (getCapability term keyDown) <|>
    fail "Terminal does not support downward arrow key."
  hdlOutSupportsANSI <- hNowSupportsANSI hdlOut
  finalMenu <-
    catch
      (hInteractWithMenu hdlIn hdlOut hdlOutSupportsANSI upCode downCode menu)
      (hHandleUserInterrupt hdlOut hdlOutSupportsANSI)
  return $ selected finalMenu

-- | Print "Aborted." on UserInterrupt and then re-throw exception
hHandleUserInterrupt :: Handle -> Bool -> AsyncException -> IO (Menu a)
hHandleUserInterrupt hdl hdlSupportsANSI UserInterrupt = do
  when hdlSupportsANSI (hSetSGR hdl [SetColor Foreground Vivid Red])
  hPutStrLn hdl "Aborted."
  when hdlSupportsANSI (hSetSGR hdl [Reset] >> hShowCursor hdl)
  throwIO UserInterrupt
hHandleUserInterrupt _ _ e = throwIO e
