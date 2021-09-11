module Yamte.Mode.Action ( actionMode ) where

import Graphics.Vty
  ( Key(KChar, KDown, KEnd, KHome, KLeft, KRight, KUp)
  , Modifier(MCtrl)
  )

-- import Yamte.AST (stepDeeper, stepShallower, stepForwards, stepBackwards)
import Yamte.Editor ( enterMode )
import Yamte.Mode ( standardActions )
import Yamte.Mode.File ( fileMode )
-- import Yamte.Mode.Input ( inputMode )
import Yamte.Types ( Action(..), Mode(ActionMode), ModifiedKey(..) )

actions :: [ Action ]
actions =
  {- [ Action { _trigger = ModifiedKey (KChar 'e') []
             , _description = "Switch to input mode"
             , _transformation = return . enterMode inputMode
             } -}
  [ Action { _trigger = ModifiedKey (KChar 'f') []
           , _description = "Switch to file mode"
           , _transformation = return . enterMode fileMode
           }
  ]

    {- , Action { _trigger = ModifiedKey (KChar 'w') []
             , _description = "Step shallower"
             , _transformation = return . stepShallower
             }
    , Action { _trigger = ModifiedKey KUp []
             , _description = "Step shallower"
             , _transformation = return . stepShallower
             }
    , Action { _trigger = ModifiedKey (KChar 's') []
             , _description = "Step deeper"
             , _transformation = return . stepDeeper
             }
    , Action { _trigger = ModifiedKey KDown []
             , _description = "Step deeper"
             , _transformation = return . stepDeeper
             }
    , Action { _trigger = ModifiedKey (KChar 'a') []
             , _description = "Step backwards"
             , _transformation = return . stepBackwards
             }
    , Action { _trigger = ModifiedKey KLeft []
             , _description = "Step backwards"
             , _transformation = return . stepBackwards
             }
    , Action { _trigger = ModifiedKey (KChar 'd') []
             , _description = "Step forwards"
             , _transformation = return . stepForwards
             }
    , Action { _trigger = ModifiedKey KRight []
             , _description = "Step forwards"
             , _transformation = return . stepForwards
             } -}
actionMode :: Mode
actionMode = ActionMode "Action" $ standardActions ++ actions
