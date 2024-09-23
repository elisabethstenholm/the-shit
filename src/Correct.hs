-- | This module contains the 'correct' function, which suggests and
--   runs a corrected command
module Correct
  ( correct
  ) where

import           Request                   (request, suggestions)
import           Shell                     (Shell)
import           TUI                       (Menu (selected), hInteract,
                                            mkMaybeMenu)

import           Control.Applicative       ((<|>))
import           Control.Applicative.Logic (convert)
import           Control.Exception         (AsyncException (UserInterrupt),
                                            catch, throwIO)
import           Control.Monad             (when)
import qualified Data.ByteString.Char8     as ByteString
import           Data.List.Extra           (unsnoc)
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
import           System.IO                 (hFlush, hPutStr, hPutStrLn, stderr)
import           System.Process            (readProcess)

correct :: String -> String -> Double -> Shell -> IO ()
correct cmds apiKeyVarName temp shell = do
  apiKey <-
    (lookupEnv apiKeyVarName >>= convert) <|>
    fail ("No environment variable " ++ apiKeyVarName ++ ".")
  aliases <- readProcess (show shell) ["-ic", "alias"] []
  (prevCmds, cmd) <-
    convert (unsnoc $ lines cmds) <|> fail "No command provided."
  response <-
    runReq defaultHttpConfig $
    request aliases cmd prevCmds (ByteString.pack apiKey) temp
  term <- setupTermFromEnv
  keypadOnCode <-
    convert (getCapability term keypadOn) <|>
    fail "Terminal does not support application keypad mode."
  hPutStr stderr keypadOnCode -- Enable application mode in terminal
  hFlush stderr
  upCode <-
    convert (getCapability term keyUp) <|>
    fail "Terminal does not support upward arrow key."
  downCode <-
    convert (getCapability term keyDown) <|>
    fail "Terminal does not support downward arrow key."
  menu <-
    convert (mkMaybeMenu $ suggestions $ responseBody response) <|>
    fail "No suggestions."
  stderrSupportsANSI <- hNowSupportsANSI stderr
  finalMenu <-
    catch
      (hInteract stderr stderrSupportsANSI upCode downCode menu)
      (handleUserInterrupt stderrSupportsANSI)
  putStrLn $ selected finalMenu

-- | Print "Aborted." on UserInterrupt and then re-throw exception
handleUserInterrupt :: Bool -> AsyncException -> IO (Menu a)
handleUserInterrupt stderrSupportsANSI UserInterrupt = do
  when stderrSupportsANSI (hSetSGR stderr [SetColor Foreground Vivid Red])
  hPutStrLn stderr "Aborted."
  when stderrSupportsANSI (hSetSGR stderr [Reset] >> hShowCursor stderr)
  throwIO UserInterrupt
handleUserInterrupt _ e = throwIO e
