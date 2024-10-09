-- | This module contains the Shell data type which represents currently
--   supported shells
module Shell
  ( Shell(..)
  , readShellWithDefault
  ) where

import           Data.Char  (toLower)
import           Data.Maybe (fromMaybe)
import           Text.Read  (readMaybe)

-- | Supported shells
data Shell
  = Bash
  | Zsh
  deriving (Eq, Ord)

instance Show Shell where
  show Bash = "bash"
  show Zsh  = "zsh"

instance Read Shell where
  readsPrec _ s =
    case map toLower s of
      "bash" -> [(Bash, "")]
      "zsh"  -> [(Zsh, "")]
      _      -> []

-- | Read Shell with Bash as default
readShellWithDefault :: Shell -> String -> Shell
readShellWithDefault s = fromMaybe s . readMaybe
