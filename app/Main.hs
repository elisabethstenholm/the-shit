module Main (main) where

import Lib
import Options.Applicative

main :: IO ()
main = someFunc

opts :: ParserInfo ()
opts =
  info
    _
    (fullDesc <> header "The Fix - correct console commands")
