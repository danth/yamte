module Yamte.Mode.File ( fileMode ) where

import Graphics.Vty ( Key(KChar), Modifier(MCtrl) )

import Yamte.Editor ( leaveMode, reloadFile, saveFile )
import Yamte.Mode ( standardActions )
import Yamte.Types ( Action(..), Mode(ActionMode), ModifiedKey(..), State )

leaveAfter :: (State -> IO State) -> State -> IO State
leaveAfter operation state = do state' <- operation state
                                return $ leaveMode state'

actions :: [ Action ]
actions = [ Action { _trigger = ModifiedKey (KChar 's') []
                   , _description = "Save file"
                   , _transformation = leaveAfter saveFile
                   }
          , Action { _trigger = ModifiedKey (KChar 'r') []
                   , _description = "Reload file"
                   , _transformation = leaveAfter reloadFile
                   }
          ]

fileMode :: Mode
fileMode = ActionMode "File" $ standardActions ++ actions
