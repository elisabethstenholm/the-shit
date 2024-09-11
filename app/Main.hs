module Main (main) where

import Lib
import Options.Applicative

main :: IO ()
main = do
  command <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) opts
  putStrLn command

parseCommand :: Parser Command
parseCommand =
  strArgument
    (metavar "COMMAND" 
    <> help "The command to be corrected")

opts :: ParserInfo Command
opts =
  info
    (parseCommand <**> helper)
    (fullDesc 
    <> progDesc "The Fix is a command line program which corrects the previous command." 
    <> header "The Fix - correct console commands")
