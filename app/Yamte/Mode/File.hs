module Yamte.Mode.File ( fileMode ) where

import Graphics.Vty ( Key(KChar), Modifier(MCtrl) )

import Yamte.Editor ( leaveMode, reloadFile, saveFile, standardActions )
import Yamte.Types ( Action(..), Mode(ActionMode), ModifiedKey(..), State )

leaveAfter :: (State -> IO State) -> State -> IO State
leaveAfter operation state = do state' <- operation state
                                return $ leaveMode state'

actions :: [ Action ]
actions
  = [ IOAction (ModifiedKey (KChar 's') []) "Save file" $ leaveAfter saveFile
    , IOAction (ModifiedKey (KChar 'r') []) "Reload file"
      $ leaveAfter reloadFile
    ]

fileMode :: Mode
fileMode = ActionMode "File" $ standardActions ++ actions
