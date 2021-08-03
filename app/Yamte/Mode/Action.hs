module Yamte.Mode.Action
  ( actionMode
  ) where

import UI.NCurses (Key(..))
import Yamte.Cursor
import Yamte.Editor (Mode, enterMode, leaveMode, saveFile)
import Yamte.Mode.GenericAction
import Yamte.Mode.Input (inputMode)

actions =
  [ (Action (Left '\^Q') leaveMode)
  , (Action (Left 'a') moveLeft)
  , (Action (Right KeyLeftArrow) moveLeft)
  , (Action (Left 'd') moveRight)
  , (Action (Right KeyRightArrow) moveRight)
  , (Action (Left 'w') moveUp)
  , (Action (Right KeyUpArrow) moveUp)
  , (Action (Left 's') moveDown)
  , (Action (Right KeyDownArrow) moveDown)
  , (Action (Left '\^W') moveTop)
  , (Action (Left '\^S') moveBottom)
  , (Action (Left '\^A') moveHome)
  , (Action (Right KeyHome) moveHome)
  , (Action (Left '\^D') moveEnd)
  , (Action (Right KeyEnd) moveEnd)
  , (Action (Left 'e') $ enterMode inputMode)
  , (IOAction (Left '\^O') saveFile)
  ]

actionMode :: Mode
actionMode = makeActionMode "Action" actions
