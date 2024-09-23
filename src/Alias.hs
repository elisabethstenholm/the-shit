{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the 'alias' function, which creates the alias for
--   correcting a command
module Alias
  ( alias
  ) where

import           Shell                           (Shell (..), toShell)

import           Data.Text.Format                (format)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import           System.Posix.Process.ByteString (getParentProcessID)
import           System.Process                  (readProcess)

alias :: String -> String -> Double -> IO ()
alias apiKeyVarName aliasName temp = do
  ppid <- getParentProcessID
  shell <-
    (toShell . filter (/= '\n')) <$>
    readProcess "ps" ["-p", show ppid, "-o", "comm="] ""
  putStrLn $ Text.unpack $ aliasText apiKeyVarName aliasName temp shell

addToHistoryCommand :: Shell -> String
addToHistoryCommand Bash = "history -s"
addToHistoryCommand Zsh  = "print -s"

-- | Create string representing bash function
aliasText :: String -> String -> Double -> Shell -> Text
aliasText apiKeyVarName aliasName temp shell =
  format
    "{} () {\n\
    \    TS_SHELL_ALIASES=$(alias);\n\
    \    export TS_SHELL_ALIASES;\n\
    \    TS_HISTORY=\"$(fc -ln -10 -2)\";\n\
    \    export TS_HISTORY;\n\
    \    TS_CMD=$(\n\
    \            the-shit correct \"$(fc -ln -1)\" -n \"{}\" -t \"{}\"\n\
    \    ) && eval \"$TS_CMD\";\n\
    \    test -n \"$TS_CMD\" && {} \"$TS_CMD\";\n\
    \    unset TS_SHELL_ALIASES;\n\
    \    unset TS_HISTORY;\n\
    \}"
    [aliasName, apiKeyVarName, show temp, addToHistoryCommand shell]
