module Yamte.Mode.File
  ( fileMode
  ) where

import Yamte.Editor (Mode, leaveMode, reloadFile, saveFile)
import Yamte.Mode.GenericAction

actions :: [Action]
actions =
  [ (Action (Left '\^Q') leaveMode)
  , (IOAction (Left 's') saveFile)
  , (IOAction (Left 'r') reloadFile)
  ]

fileMode :: Mode
fileMode = makeActionMode "File" actions
