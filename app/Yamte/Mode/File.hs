module Yamte.Mode.File
  ( fileMode
  ) where

import Graphics.Vty (Key(KChar), Modifier(MCtrl))
import Yamte.Editor (leaveMode, reloadFile, saveFile)
import Yamte.Types (Action(..), Mode(ActionMode), ModifiedKey(..), State)

leaveAfter :: (State -> IO State) -> State -> IO State
leaveAfter operation state = do
  state' <- operation state
  return $ leaveMode state'

actions :: [Action]
actions =
  [ Action (ModifiedKey (KChar 'Q') [MCtrl]) leaveMode
  , IOAction (ModifiedKey (KChar 's') []) $ leaveAfter saveFile
  , IOAction (ModifiedKey (KChar 'r') []) $ leaveAfter reloadFile
  ]

fileMode :: Mode
fileMode = ActionMode "File" actions
