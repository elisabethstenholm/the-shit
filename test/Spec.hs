import           TUI                   (Direction (..), Menu (..),
                                        directionList, hInteractWithMenu,
                                        menuList)
import           UserInteractions      (userInteractions)

import           System.IO             (IOMode (ReadMode, WriteMode), hClose,
                                        hPutStr, openFile)
import           Test.Hspec
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

mockUserInteractionWithMenu ::
     String -> String -> String -> Menu String -> IO (Menu String)
mockUserInteractionWithMenu upCode downCode userInput startMenu = do
  hdlIn <- openFile "test/mock_stdin.txt" WriteMode
  hPutStr hdlIn userInput
  hClose hdlIn
  hdlIn <- openFile "test/mock_stdin.txt" ReadMode
  hdlOut <- openFile "test/mock_stderr.txt" WriteMode
  endMenu <- hInteractWithMenu hdlIn hdlOut True upCode downCode startMenu
  hClose hdlIn
  hClose hdlOut
  return endMenu

testHInteractWithMenu ::
     (String, String, String, Menu String, Menu String) -> Spec
testHInteractWithMenu (upCode, downCode, userInput, startMenu, endMenu) = do
  menu <-
    runIO $ mockUserInteractionWithMenu upCode downCode userInput startMenu
  it
    ("selects " ++ selected endMenu ++ " when doing " ++ show userInput ++
     " on " ++
     show startMenu) $
    menu `shouldBe`
    endMenu

instance Arbitrary Direction where
  arbitrary = do
    b <- arbitrary
    return $
      if b
        then Up
        else Down

keyToCode :: String -> String -> Direction -> String
keyToCode upCode _ Up     = upCode
keyToCode _ downCode Down = downCode

prop_directionListCorrectlyReadsInput ::
     [Direction] -> String -> String -> Property
prop_directionListCorrectlyReadsInput keys upCode downCode =
  let input = concat $ keyToCode upCode downCode <$> keys
   in not (null upCode) && not ('\n' `elem` upCode) && not (null downCode) &&
      not ('\n' `elem` downCode) &&
      upCode /=
      downCode ==>
      directionList upCode downCode input ==
      keys

newtype MenuAtTop a =
  MenuAtTop (Menu a)
  deriving (Eq)

instance Show a => Show (MenuAtTop a) where
  show (MenuAtTop menu) = show menu

instance Arbitrary a => Arbitrary (MenuAtTop a) where
  arbitrary = do
    s <- resize 30 arbitrary
    b <- resize 5 $ listOf $ resize 30 arbitrary
    return $ MenuAtTop $ Menu {over = [], selected = s, below = b}

newtype MenuAtBottom a =
  MenuAtBottom (Menu a)
  deriving (Eq)

instance Show a => Show (MenuAtBottom a) where
  show (MenuAtBottom menu) = show menu

instance Arbitrary a => Arbitrary (MenuAtBottom a) where
  arbitrary = do
    o <- resize 5 $ listOf $ resize 30 arbitrary
    s <- resize 30 arbitrary
    return $ MenuAtBottom $ Menu {over = o, selected = s, below = []}

prop_UpWhenAtTopOfMenu :: MenuAtTop String -> Int -> Bool
prop_UpWhenAtTopOfMenu (MenuAtTop menu) n =
  let keys = replicate n Up
   in menuList menu keys == menu : replicate n menu

prop_UpWhenAtBottomOfMenu :: MenuAtBottom String -> Int -> Bool
prop_UpWhenAtBottomOfMenu (MenuAtBottom menu) n =
  let keys = replicate n Down
   in menuList menu keys == menu : replicate n menu

main :: IO ()
main = do
  unitTestTree <-
    testSpec "Unit tests" $ describe "hInteractWithMenu" $ sequence_ $
    testHInteractWithMenu <$>
    userInteractions
  let propTestTree =
        testGroup
          "Property tests"
          [ testProperty
              "directionList correctly reads input"
              prop_directionListCorrectlyReadsInput
          , testProperty
              "nothing happens when going up when at top of menu"
              prop_UpWhenAtTopOfMenu
          , testProperty
              "nothing happens when going down when at bottom of menu"
              prop_UpWhenAtBottomOfMenu
          ]
  defaultMain $ testGroup "All tests" [unitTestTree, propTestTree]
