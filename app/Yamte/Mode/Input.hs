module Yamte.Mode.Input (inputMode) where

import Yamte.Editor (Mode(Mode), Action(Action), leaveMode)
import Yamte.Cursor
import UI.NCurses (Key(..))

inputMode :: Mode
inputMode = Mode "Input" [ (Action (Left '\^Q') leaveMode)
                         , (Action (Right KeyLeftArrow) moveLeft)
                         , (Action (Right KeyRightArrow) moveRight)
                         , (Action (Right KeyUpArrow) moveUp)
                         , (Action (Right KeyDownArrow) moveDown)
                         , (Action (Right KeyHome) moveHome)
                         , (Action (Right KeyEnd) moveEnd)
                         ]
