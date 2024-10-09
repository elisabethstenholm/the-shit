-- | This module contains utility functions
module Utils
  ( uncons
  ) where

import           Control.Applicative (Alternative (..))

-- | Generalized uncons
uncons :: (Alternative f) => [a] -> f (a, [a])
uncons []     = empty
uncons (x:xs) = pure (x, xs)
