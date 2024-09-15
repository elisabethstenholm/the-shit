-- | This module contains the 'alias' function, which creates the alias for
--   fixing a command
module Alias
  ( alias
  ) where

alias :: String -> String -> IO ()
alias = (putStrLn .) . aliasString

-- | Create string representing bash function
aliasString :: String -> String -> String
aliasString apiKeyVarName aliasName =
  aliasName ++
  " () {\n" ++
  "\tTF_TMP=$(mktemp /tmp/thefix_suggestion.XXXXXX);\n" ++
  "\texport TF_TMP;\n" ++
  "\tthe-fix correct \"$(fc -ln -1)\" -n \"" ++
  apiKeyVarName ++
  "\" && {\n" ++
  "\t\tTF_CMD=$(cat \"$TF_TMP\");\n" ++
  "\t\teval \"$TF_CMD\";\n" ++
  "\t\tprint -s \"$TF_CMD\";\n" ++
  "\t\tunset TF_CMD;\n" ++ "\t};\n" ++ "\tunset TF_TMP;\n" ++ "}"
