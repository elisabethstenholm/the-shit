{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the 'alias' function, which creates the alias for
--   correcting a command
module Alias
  ( alias
  ) where

import           Shell            (Shell (..))

import           Data.Text.Format (format)
import           Data.Text.Lazy   (Text)
import qualified Data.Text.Lazy   as Text

alias :: String -> String -> Shell -> IO ()
alias = (((putStrLn . Text.unpack) .) .) . aliasText

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
