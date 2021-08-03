module Yamte.Mode.File
  ( fileMode
  ) where

import Yamte.Editor (Mode, State, leaveMode, reloadFile, saveFile)
import Yamte.Mode.GenericAction

leaveAfter :: (State -> IO State) -> State -> IO State
leaveAfter operation state = do
  state' <- operation state
  return $ leaveMode state'

actions :: [Action]
actions =
  [ (Action (Left '\^Q') leaveMode)
  , (IOAction (Left 's') $ leaveAfter saveFile)
  , (IOAction (Left 'r') $ leaveAfter reloadFile)
  ]

fileMode :: Mode
fileMode = makeActionMode "File" actions
