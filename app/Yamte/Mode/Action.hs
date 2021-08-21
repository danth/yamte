module Yamte.Mode.Action ( actionMode ) where

import Graphics.Vty
  ( Key(KChar, KDown, KEnd, KHome, KLeft, KRight, KUp)
  , Modifier(MCtrl)
  )

import Yamte.Cursor
import Yamte.Editor ( enterMode )
import Yamte.Mode ( standardActions )
import Yamte.Mode.File ( fileMode )
import Yamte.Mode.Input ( inputMode )
import Yamte.Types ( Action(..), Mode(ActionMode), ModifiedKey(..) )

actions :: [ Action ]
actions
  = [ Action { _trigger = ModifiedKey (KChar 'e') []
             , _description = "Switch to input mode"
             , _transformation = return . enterMode inputMode
             }
    , Action { _trigger = ModifiedKey (KChar 'f') []
             , _description = "Switch to file mode"
             , _transformation = return . enterMode fileMode
             }
    , Action { _trigger = ModifiedKey (KChar 'w') []
             , _description = "Move up one line"
             , _transformation = return . moveUp
             }
    , Action { _trigger = ModifiedKey KUp []
             , _description = "Move up one line"
             , _transformation = return . moveUp
             }
    , Action { _trigger = ModifiedKey (KChar 's') []
             , _description = "Move down one line"
             , _transformation = return . moveDown
             }
    , Action { _trigger = ModifiedKey KDown []
             , _description = "Move down one line"
             , _transformation = return . moveDown
             }
    , Action { _trigger = ModifiedKey (KChar 'a') []
             , _description = "Move left one column"
             , _transformation = return . moveLeft
             }
    , Action { _trigger = ModifiedKey KLeft []
             , _description = "Move left one column"
             , _transformation = return . moveLeft
             }
    , Action { _trigger = ModifiedKey (KChar 'd') []
             , _description = "Move right one column"
             , _transformation = return . moveRight
             }
    , Action { _trigger = ModifiedKey KRight []
             , _description = "Move right one column"
             , _transformation = return . moveRight
             }
    , Action { _trigger = ModifiedKey (KChar 'w') [ MCtrl ]
             , _description = "Move to the start of the file"
             , _transformation = return . moveTop
             }
    , Action { _trigger = ModifiedKey (KChar 's') [ MCtrl ]
             , _description = "Move to the end of the file"
             , _transformation = return . moveBottom
             }
    , Action { _trigger = ModifiedKey (KChar 'a') [ MCtrl ]
             , _description = "Move to the start of the line"
             , _transformation = return . moveHome
             }
    , Action { _trigger = ModifiedKey KHome []
             , _description = "Move to the start of the line"
             , _transformation = return . moveHome
             }
    , Action { _trigger = ModifiedKey (KChar 'd') [ MCtrl ]
             , _description = "Move to the end of the line"
             , _transformation = return . moveEnd
             }
    , Action { _trigger = ModifiedKey KEnd []
             , _description = "Move to the end of the line"
             , _transformation = return . moveEnd
             }
    ]

actionMode :: Mode
actionMode = ActionMode "Action" $ standardActions ++ actions
