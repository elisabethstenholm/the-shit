{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the 'alias' function, which creates the alias for
--   correcting a command
module Alias
  ( alias
  , AliasArgs(..)
  ) where

import           Shell                           (Shell (..),
                                                  readShellWithDefault)

import           System.Posix.Process.ByteString (getParentProcessID)
import           System.Process                  (readProcess)
import           Text.Printf                     (printf)

-- | Arguments needed by the 'alias' function
data AliasArgs =
  AliasArgs
    { apiKeyVarName :: String
    , aliasName     :: String
    , temperature   :: Double
    }
  deriving (Eq, Show)

alias :: AliasArgs -> IO ()
alias args = do
  ppid <- getParentProcessID
  shell <-
    (readShellWithDefault Bash . filter (/= '\n')) <$>
    readProcess "ps" ["-p", show ppid, "-o", "comm="] ""
  putStrLn $ aliasString args shell

addToHistoryCommand :: Shell -> String
addToHistoryCommand Bash = "history -s"
addToHistoryCommand Zsh  = "print -s"

-- | Create string representing bash function
aliasString :: AliasArgs -> Shell -> String
aliasString args shell =
  printf
    "%s () {\n\
    \    TS_SHELL_ALIASES=$(alias);\n\
    \    export TS_SHELL_ALIASES;\n\
    \    TS_HISTORY=\"$(fc -ln -10 -2)\";\n\
    \    export TS_HISTORY;\n\
    \    TS_CMD=$(\n\
    \            the-shit correct \"$(fc -ln -1)\" -n \"%s\" -t \"%f\"\n\
    \    ) && eval \"$TS_CMD\";\n\
    \    test -n \"$TS_CMD\" && %s \"$TS_CMD\";\n\
    \    unset TS_SHELL_ALIASES;\n\
    \    unset TS_HISTORY;\n\
    \}"
    (aliasName args)
    (apiKeyVarName args)
    (temperature args)
    (addToHistoryCommand shell)
