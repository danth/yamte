module Yamte.Mode.Action (actionMode) where

import Yamte.Editor (Mode(Mode), Action(Action), leaveMode)
import Yamte.Cursor
import UI.NCurses (Key(..))

actionMode :: Mode
actionMode = Mode "Action" [ (Action (Left '\^Q') leaveMode)
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
                           ]
