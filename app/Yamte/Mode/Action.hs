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
import Yamte.Mode.GenericAction (makeActionMode)
import Yamte.Mode.Input (inputMode)
import Yamte.Types (Action(..), Mode)

actions :: [Action]
actions =
  [ Action (KChar 'q', [MCtrl]) leaveMode
  , Action (KChar 'a', []) moveLeft
  , Action (KLeft, []) moveLeft
  , Action (KChar 'd', []) moveRight
  , Action (KRight, []) moveRight
  , Action (KChar 'w', []) moveUp
  , Action (KUp, []) moveUp
  , Action (KChar 's', []) moveDown
  , Action (KDown, []) moveDown
  , Action (KChar 'w', [MCtrl]) moveTop
  , Action (KChar 's', [MCtrl]) moveBottom
  , Action (KChar 'a', [MCtrl]) moveHome
  , Action (KHome, []) moveHome
  , Action (KChar 'd', [MCtrl]) moveEnd
  , Action (KEnd, []) moveEnd
  , Action (KChar 'e', []) $ enterMode inputMode
  , Action (KChar 'f', []) $ enterMode fileMode
  ]

actionMode :: Mode
actionMode = makeActionMode "Action" actions
