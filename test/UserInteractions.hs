module UserInteractions where

import           TUI (Menu (..))

userInteractions :: [(String, String, String, Menu String, Menu String)]
userInteractions =
  [ ( "u"
    , "d"
    , "udd\n"
    , (Menu {over = [], selected = "python", below = ["python3"]})
    , (Menu {over = ["python"], selected = "python3", below = []}))
  , ( "up"
    , "down"
    , "uphgjkeosdowndownghous987uphg87y\nlkjhupsdowng78e\n\nhwdownaue98"
    , (Menu
         { over = ["alternative1", "alternative2"]
         , selected = "alternative3"
         , below = []
         })
    , (Menu
         { over = ["alternative1"]
         , selected = "alternative2"
         , below = ["alternative3"]
         }))
  , ( "k"
    , "j"
    , "kjjkkjjkjk\nasdfghjklqwerty" --
    , Menu
        { over = ["opt1", "opt2", "opt3"]
        , selected = "opt4"
        , below = ["opt5", "opt6"]
        }
    , Menu
        { over = ["opt1", "opt2", "opt3"]
        , selected = "opt4"
        , below = ["opt5", "opt6"]
        })
  , ( "\ESC[A"
    , "\ESC[B"
    , "\ESC[B\ESC[B\ESC[A\nzxcvbnmasdf"
    , Menu
        { over = ["apple", "banana"]
        , selected = "cherry"
        , below = ["date", "elderberry"]
        }
    , Menu
        { over = ["apple", "banana", "cherry"]
        , selected = "date"
        , below = ["elderberry"]
        })
  , ( "w"
    , "s"
    , "wswswsww\nqwepoiuytnm"
    , Menu
        { over = ["itemA", "itemB"]
        , selected = "itemC"
        , below = ["itemD", "itemE"]
        }
    , Menu
        { over = []
        , selected = "itemA"
        , below = ["itemB", "itemC", "itemD", "itemE"]
        })
  , ( "↑"
    , "↓"
    , "↓↓↑↓\nlkjhgfdsamnbvcx"
    , Menu {over = ["one", "two"], selected = "three", below = ["four", "five"]}
    , Menu
        {over = ["one", "two", "three", "four"], selected = "five", below = []})
  , ( "u"
    , "d"
    , "dduuudd\nxyzqwertyuiop"
    , Menu
        {over = ["x", "y", "z"], selected = "alpha", below = ["beta", "gamma"]}
    , Menu
        {over = ["x", "y", "z", "alpha"], selected = "beta", below = ["gamma"]})
  , ( "\ESCOA"
    , "\ESCOB"
    , "\ESCOB\ESCOA\ESCOB\ESCOB\nplokmijnuhbygv"
    , Menu
        { over = ["first", "second", "third"]
        , selected = "fourth"
        , below = ["fifth", "sixth"]
        }
    , Menu
        { over = ["first", "second", "third", "fourth", "fifth"]
        , selected = "sixth"
        , below = []
        })
  ]
