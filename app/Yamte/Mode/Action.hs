module Yamte.Mode.Action
  ( actionMode
  ) where

import Graphics.Vty
  ( Key(KChar, KDown, KEnd, KHome, KLeft, KRight, KUp)
  , Modifier(MCtrl)
  )
import Yamte.Cursor
import Yamte.Editor (enterMode, leaveMode)
import Yamte.Mode.File (fileMode)
import Yamte.Mode.Input (inputMode)
import Yamte.Types (Action(..), Mode(ActionMode), ModifiedKey(..))

actions :: [Action]
actions =
  [ Action (ModifiedKey (KChar 'q') [MCtrl]) leaveMode
  , Action (ModifiedKey (KChar 'a') []) moveLeft
  , Action (ModifiedKey KLeft []) moveLeft
  , Action (ModifiedKey (KChar 'd') []) moveRight
  , Action (ModifiedKey KRight []) moveRight
  , Action (ModifiedKey (KChar 'w') []) moveUp
  , Action (ModifiedKey KUp []) moveUp
  , Action (ModifiedKey (KChar 's') []) moveDown
  , Action (ModifiedKey KDown []) moveDown
  , Action (ModifiedKey (KChar 'w') [MCtrl]) moveTop
  , Action (ModifiedKey (KChar 's') [MCtrl]) moveBottom
  , Action (ModifiedKey (KChar 'a') [MCtrl]) moveHome
  , Action (ModifiedKey KHome []) moveHome
  , Action (ModifiedKey (KChar 'd') [MCtrl]) moveEnd
  , Action (ModifiedKey KEnd []) moveEnd
  , Action (ModifiedKey (KChar 'e') []) $ enterMode inputMode
  , Action (ModifiedKey (KChar 'f') []) $ enterMode fileMode
  ]

actionMode :: Mode
actionMode = ActionMode "Action" actions
