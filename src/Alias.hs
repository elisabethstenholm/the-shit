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

alias :: String -> String -> Double -> Shell -> IO ()
alias = ((((putStrLn . Text.unpack) .) .) .) . aliasText

addToHistoryCommand :: Shell -> String
addToHistoryCommand Bash = "history -s"
addToHistoryCommand Zsh  = "print -s"

-- | Create string representing bash function
aliasText :: String -> String -> Double -> Shell -> Text
aliasText apiKeyVarName aliasName temp shell =
  format
    "{} () {\n\
    \    TF_CMD=$(\n\
    \            the-shit correct \"$(fc -ln -10)\" -n \"{}\" -t \"{}\"\n\
    \    ) && eval \"$TF_CMD\";\n\
    \    test -n \"$TF_CMD\" && {} \"$TF_CMD\";\n\
    \}"
    [aliasName, apiKeyVarName, show temp, addToHistoryCommand shell]
