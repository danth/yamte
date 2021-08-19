module Yamte.Mode.Action
  ( actionMode
  ) where

import Graphics.Vty
  ( Key(KChar, KDown, KEnd, KHome, KLeft, KRight, KUp)
  , Modifier(MCtrl)
  )
import Yamte.Cursor
import Yamte.Editor (enterMode, standardActions)
import Yamte.Mode.File (fileMode)
import Yamte.Mode.Input (inputMode)
import Yamte.Types (Action(..), Mode(ActionMode), ModifiedKey(..))

actions :: [Action]
actions =
  [ Action (ModifiedKey (KChar 'e') []) "Switch to input mode" $
    enterMode inputMode
  , Action (ModifiedKey (KChar 'f') []) "Switch to file mode" $
    enterMode fileMode
  , Action (ModifiedKey (KChar 'w') []) "Move up one line" moveUp
  , Action (ModifiedKey KUp []) "Move up one line" moveUp
  , Action (ModifiedKey (KChar 's') []) "Move down one line" moveDown
  , Action (ModifiedKey KDown []) "Move down one line" moveDown
  , Action (ModifiedKey (KChar 'a') []) "Move left one column" moveLeft
  , Action (ModifiedKey KLeft []) "Move left one column" moveLeft
  , Action (ModifiedKey (KChar 'd') []) "Move right one column" moveRight
  , Action (ModifiedKey KRight []) "Move right one column" moveRight
  , Action
      (ModifiedKey (KChar 'w') [MCtrl])
      "Move to the start of the file"
      moveTop
  , Action
      (ModifiedKey (KChar 's') [MCtrl])
      "Move to the end of the file"
      moveBottom
  , Action
      (ModifiedKey (KChar 'a') [MCtrl])
      "Move to the start of the line"
      moveHome
  , Action (ModifiedKey KHome []) "Move to the start of the line" moveHome
  , Action
      (ModifiedKey (KChar 'd') [MCtrl])
      "Move to the end of the line"
      moveEnd
  , Action (ModifiedKey KEnd []) "Move to the end of the line" moveEnd
  ]

actionMode :: Mode
actionMode = ActionMode "Action" $ standardActions ++ actions
