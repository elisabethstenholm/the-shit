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
  "\tTF_CMD=$(\n" ++
  "\t\tthe-fix correct \"$(fc -ln -1)\" -n \"" ++
  apiKeyVarName ++
  "\"\n" ++
  "\t) && eval \"$TF_CMD\";\n" ++
  "\ttest -n \"$TF_CMD\" && print -s $TF_CMD;\n" ++ "}"
