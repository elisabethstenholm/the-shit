{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the 'alias' function, which creates the alias for
--   correcting a command
module Alias
  ( alias
  ) where

import           Data.Char                       (toLower)
import           Data.Maybe                      (fromMaybe)
import           Data.Text.Format                (format)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import           System.Posix.Process.ByteString (getParentProcessID)
import           System.Process                  (readProcess)
import           Text.Read                       (readMaybe)

-- | Supported shells
data Shell
  = Bash
  | Zsh
  deriving (Eq, Ord, Show)

instance Read Shell where
  readsPrec _ s =
    case map toLower s of
      "bash" -> [(Bash, "")]
      "zsh"  -> [(Zsh, "")]
      _      -> []

alias :: String -> String -> IO ()
alias apiKeyVarName aliasName = do
  ppid <- getParentProcessID
  shell <-
    filter (/= '\n') <$> readProcess "ps" ["-p", show ppid, "-o", "comm="] ""
  putStrLn $ Text.unpack $ aliasText apiKeyVarName aliasName (toShell shell)

-- | Read Shell with Bash as default
toShell :: String -> Shell
toShell = fromMaybe Bash . readMaybe

addToHistoryCommand :: Shell -> String
addToHistoryCommand Bash = "history -s"
addToHistoryCommand Zsh  = "print -s"

-- | Create string representing bash function
aliasText :: String -> String -> Shell -> Text
aliasText apiKeyVarName aliasName shell =
  format
    "{} () {\n\
    \    TF_CMD=$(\n\
    \            the-shit correct \"$(fc -ln -1)\" -n \"{}\"\n\
    \    ) && eval \"$TF_CMD\";\n\
    \    test -n \"$TF_CMD\" && {} \"$TF_CMD\";\n\
    \}"
    [aliasName, apiKeyVarName, addToHistoryCommand shell]
