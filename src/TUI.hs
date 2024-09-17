-- | Module containing the terminal user interface (TUI)
module TUI
  ( Menu(..)
  , mkMaybeMenu
  , hInteract
  ) where

import           Control.Monad
import           Data.List           (isPrefixOf)
import           System.Console.ANSI
import           System.IO

-- | Menu items together with information about currently selected
data Menu a =
  Menu
    { over     :: [a] -- ^Elements over selected element
    , selected :: a -- ^Selected element
    , below    :: [a] -- ^Elements below selected element
    }
  deriving (Eq, Ord, Show)

-- | Make 'Menu' from list with first element as selected
mkMaybeMenu :: [a] -> Maybe (Menu a)
mkMaybeMenu []     = Nothing
mkMaybeMenu (x:xs) = Just $ Menu {over = [], selected = x, below = xs}

-- | Length of a menu
menuLength :: Menu a -> Int
menuLength menu = length (over menu) + length (below menu) + 1

-- | User interaction with a menu, returning the last state
hInteract :: Handle -> String -> String -> Menu String -> IO (Menu String)
hInteract hdl upCode downCode menu = do
  hHideCursor hdl
  hSetEcho stdin False
  hPrintMenu hdl menu
  hSetBuffering stdin NoBuffering
  input <- getContents
  let menus = menuList menu (keyList upCode downCode input)
  if null menus
    then do
      hShowCursor hdl
      return menu
    else do
      let n = menuLength menu
      sequence_ $
        ((hCursorUpLine hdl n >> hClearFromCursorToScreenEnd hdl >>) .
         hPrintMenu hdl) <$>
        menus
      hShowCursor hdl
      return $ last menus

-- | Up or down key on keyboard
data Key
  = UpKey
  | DownKey
  deriving (Eq, Ord, Show)

-- | Move up in menu if possible
moveUp :: Menu a -> Menu a
moveUp menu@(Menu {over = [], selected = _, below = _}) = menu
moveUp (Menu {over = xs, selected = y, below = ys}) =
  Menu {over = init xs, selected = last xs, below = y : ys}

-- | Move down in menu if possible
moveDown :: Menu a -> Menu a
moveDown menu@(Menu {over = _, selected = _, below = []}) = menu
moveDown (Menu {over = xs, selected = x, below = (y:ys)}) =
  Menu {over = xs ++ [x], selected = y, below = ys}

-- | Move according to key
move :: Key -> Menu a -> Menu a
move UpKey   = moveUp
move DownKey = moveDown

-- | Read list of keys from input string
keyList :: String -> String -> String -> [Key]
keyList _ _ "" = []
keyList upCode downCode input
  | "\n" `isPrefixOf` input = []
  | upCode `isPrefixOf` input =
    UpKey : keyList upCode downCode (drop (length upCode) input)
  | downCode `isPrefixOf` input =
    DownKey : keyList upCode downCode (drop (length downCode) input)
  | otherwise = keyList upCode downCode (tail input)

-- | Apply moves to menu according to key list
menuList :: Menu a -> [Key] -> [Menu a]
menuList _ [] = []
menuList menu (k:ks) =
  let newMenu = move k menu
   in newMenu : menuList newMenu ks

-- | Print menu to given handle
hPrintMenu :: Handle -> Menu String -> IO ()
hPrintMenu hdl (Menu {over = xs, selected = s, below = ys}) = do
  mconcat $ fmap (hPutStrLn hdl) xs
  hPutStr hdl s
  hdlSupportsAnsi <- hNowSupportsANSI hdl
  hPrintSelector hdl hdlSupportsAnsi (not $ null xs) (not $ null ys)
  mconcat $ fmap (hPutStrLn hdl) ys
  hFlush hdl

-- | Print selector to given handle
hPrintSelector :: Handle -> Bool -> Bool -> Bool -> IO ()
hPrintSelector hdl hdlSupportsAnsi isBelow isOver = do
  hPutStr hdl " ["
  when hdlSupportsAnsi $ hSetSGR hdl [SetColor Foreground Vivid Green]
  hPutStr hdl "Enter"
  when hdlSupportsAnsi $ hSetSGR hdl [Reset]
  hPutStr hdl "/"
  when isBelow $ hPutStr hdl "↑/"
  when isOver $ hPutStr hdl "↓/"
  when hdlSupportsAnsi $ hSetSGR hdl [SetColor Foreground Vivid Red]
  hPutStr hdl "Ctrl+C"
  when hdlSupportsAnsi $ hSetSGR hdl [Reset]
  hPutStrLn hdl "]"
