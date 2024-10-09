-- | Module containing the terminal user interface (TUI)
module TUI
  ( Menu(..)
  , Direction(..)
  , makeMenu
  , hInteractWithMenu
  , directionList
  , menuList
  ) where

import           Utils               (uncons)

import           Control.Applicative (Alternative)
import           Control.Monad       (when)
import           Data.List           (isPrefixOf)
import           System.Console.ANSI (Color (Green, Red),
                                      ColorIntensity (Vivid),
                                      ConsoleLayer (Foreground),
                                      SGR (Reset, SetColor),
                                      hClearFromCursorToScreenEnd,
                                      hCursorUpLine, hHideCursor, hSetSGR,
                                      hShowCursor)
import           System.IO           (BufferMode (NoBuffering), Handle, hFlush,
                                      hGetContents, hPutStr, hPutStrLn,
                                      hSetBuffering, hSetEcho)

-- | Menu items together with information about currently selected
data Menu a =
  Menu
    { over     :: [a] -- ^Items over selected item
    , selected :: a -- ^Selected item
    , below    :: [a] -- ^Items below selected item
    }
  deriving (Eq, Show)

-- | Make 'Menu' from list with first item as selected
makeMenu :: (Alternative f) => [a] -> f (Menu a)
makeMenu = fmap (uncurry $ Menu []) . uncons

-- | Length of a menu
menuLength :: Menu a -> Int
menuLength menu = length (over menu) + length (below menu) + 1

-- | Direction for movement in menu (up or down)
data Direction
  = Up
  | Down
  deriving (Eq, Show)

-- | Move up in menu if possible
moveUp :: Menu a -> Menu a
moveUp menu@(Menu {over = []}) = menu
moveUp (Menu {over = xs, selected = y, below = ys}) =
  Menu {over = init xs, selected = last xs, below = y : ys}

-- | Move down in menu if possible
moveDown :: Menu a -> Menu a
moveDown menu@(Menu {below = []}) = menu
moveDown (Menu {over = xs, selected = x, below = (y:ys)}) =
  Menu {over = xs ++ [x], selected = y, below = ys}

-- | Move according to key
move :: Direction -> Menu a -> Menu a
move Up   = moveUp
move Down = moveDown

-- | Read list of keys from input string
directionList :: String -> String -> String -> [Direction]
directionList _ _ "" = []
directionList upCode downCode input
  | "\n" `isPrefixOf` input = []
  | upCode `isPrefixOf` input =
    Up : directionList upCode downCode (drop (length upCode) input)
  | downCode `isPrefixOf` input =
    Down : directionList upCode downCode (drop (length downCode) input)
  | otherwise = directionList upCode downCode (tail input)

-- | Apply moves to menu according to key list
menuList :: Menu a -> [Direction] -> [Menu a]
menuList = scanl (flip move)

-- | User interaction with a menu, returning the last state
hInteractWithMenu ::
     Handle
  -> Handle
  -> Bool
  -> String
  -> String
  -> Menu String
  -> IO (Menu String)
hInteractWithMenu hdlIn hdlOut hdlOutSupportsANSI upCode downCode menu = do
  hHideCursor hdlOut
  hSetEcho hdlIn False
  hPrintMenu hdlOut hdlOutSupportsANSI menu
  hSetBuffering hdlIn NoBuffering
  input <- hGetContents hdlIn
  let menus = menuList menu (directionList upCode downCode input)
  let n = menuLength menu
  sequence_ $
    ((hCursorUpLine hdlOut n >> hClearFromCursorToScreenEnd hdlOut >>) .
     hPrintMenu hdlOut hdlOutSupportsANSI) <$>
    menus
  hShowCursor hdlOut
  return $ last menus -- last is safe here since menus is built using scanl which produces a non-empty list

-- | Print menu to given handle
hPrintMenu :: Handle -> Bool -> Menu String -> IO ()
hPrintMenu hdl hdlSupportsANSI (Menu {over = xs, selected = s, below = ys}) = do
  mconcat $ fmap (hPutStrLn hdl) xs
  hPutStr hdl s
  hPrintSelector hdl hdlSupportsANSI (not $ null xs) (not $ null ys)
  mconcat $ fmap (hPutStrLn hdl) ys
  hFlush hdl

-- | Print selector to given handle
hPrintSelector :: Handle -> Bool -> Bool -> Bool -> IO ()
hPrintSelector hdl hdlSupportsANSI isBelow isOver = do
  hPutStr hdl " ["
  when hdlSupportsANSI $ hSetSGR hdl [SetColor Foreground Vivid Green]
  hPutStr hdl "Enter"
  when hdlSupportsANSI $ hSetSGR hdl [Reset]
  hPutStr hdl "/"
  when isBelow $ hPutStr hdl "↑/"
  when isOver $ hPutStr hdl "↓/"
  when hdlSupportsANSI $ hSetSGR hdl [SetColor Foreground Vivid Red]
  hPutStr hdl "Ctrl+C"
  when hdlSupportsANSI $ hSetSGR hdl [Reset]
  hPutStrLn hdl "]"
