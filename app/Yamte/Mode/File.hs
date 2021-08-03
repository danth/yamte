module Yamte.Mode.File
  ( fileMode
  ) where

import Yamte.Editor (Mode, reloadFile, saveFile, leaveMode)
import Yamte.Mode.GenericAction

actions :: [Action]
actions =
  [ (Action (Left '\^Q') leaveMode)
  , (IOAction (Left 's') saveFile)
  , (IOAction (Left 'r') reloadFile)
  ]

fileMode :: Mode
fileMode = makeActionMode "File" actions
