module Yamte.Mode.File
  ( fileMode
  ) where

import Graphics.Vty (Key(KChar), Modifier(MCtrl))
import Yamte.Editor (leaveMode, reloadFile, saveFile)
import Yamte.Mode.GenericAction (makeActionMode)
import Yamte.Types (Action(..), Mode, State)

leaveAfter :: (State -> IO State) -> State -> IO State
leaveAfter operation state = do
  state' <- operation state
  return $ leaveMode state'

actions :: [Action]
actions =
  [ (Action (KChar 'Q', [MCtrl]) leaveMode)
  , (IOAction (KChar 's', []) $ leaveAfter saveFile)
  , (IOAction (KChar 'r', []) $ leaveAfter reloadFile)
  ]

fileMode :: Mode
fileMode = makeActionMode "File" actions
