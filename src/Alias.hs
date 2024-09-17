{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the 'alias' function, which creates the alias for
--   fixing a command
module Alias
  ( alias
  ) where

import           Data.Text.Format
import           Data.Text.Lazy as Text

alias :: String -> String -> IO ()
alias = ((putStrLn . Text.unpack) .) . aliasString

-- | Create string representing bash function
aliasString :: String -> String -> Text
aliasString apiKeyVarName aliasName =
  format
    "{} () {\n\
    \    TF_CMD=$(\n\
    \            the-fix correct \"$(fc -ln -1)\" -n \"{}\"\n\
    \    ) && eval \"$TF_CMD\";\n\
    \    test -n \"$TF_CMD\" && print -s $TF_CMD;\n\
    \}"
    [aliasName, apiKeyVarName]

{-   aliasName ++
  " () {\n" ++
  "\tTF_CMD=$(\n" ++
  "\t\tthe-fix correct \"$(fc -ln -1)\" -n \"" ++
  apiKeyVarName ++
  "\"\n" ++
  "\t) && eval \"$TF_CMD\";\n" ++
  "\ttest -n \"$TF_CMD\" && print -s $TF_CMD;\n" ++ "}" -}
