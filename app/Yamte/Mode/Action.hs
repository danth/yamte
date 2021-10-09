module Yamte.Mode.Action ( actionMode ) where

import Data.Maybe ( fromMaybe )
import Data.Tree.Cursor ( moveUp, moveDown, moveLeft, moveRight )

import Graphics.Vty ( Key(KChar, KDown, KLeft, KRight, KUp) )

import Lens.Micro ( ASetter, over )

import Yamte.Editor ( enterMode )
import Yamte.Mode ( standardActions )
import Yamte.Mode.File ( fileMode )
import Yamte.Mode.Input ( inputMode )
import Yamte.Types ( Action(..), Mode(ActionMode), ModifiedKey(..), document )

(%?) :: ASetter s t a a -> (a -> Maybe a) -> s -> t
l %? f = over l $ \x -> fromMaybe x (f x)

infixr 4 %?

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
             , _description = "Step shallower"
             , _transformation = return . (document %? moveUp)
             }
    , Action { _trigger = ModifiedKey KUp []
             , _description = "Step shallower"
             , _transformation = return . (document %? moveUp)
             }
    , Action { _trigger = ModifiedKey (KChar 's') []
             , _description = "Step deeper"
             , _transformation = return . (document %? moveDown)
             }
    , Action { _trigger = ModifiedKey KDown []
             , _description = "Step deeper"
             , _transformation = return . (document %? moveDown)
             }
    , Action { _trigger = ModifiedKey (KChar 'a') []
             , _description = "Step backwards"
             , _transformation = return . (document %? moveLeft)
             }
    , Action { _trigger = ModifiedKey KLeft []
             , _description = "Step backwards"
             , _transformation = return . (document %? moveLeft)
             }
    , Action { _trigger = ModifiedKey (KChar 'd') []
             , _description = "Step forwards"
             , _transformation = return . (document %? moveRight)
             }
    , Action { _trigger = ModifiedKey KRight []
             , _description = "Step forwards"
             , _transformation = return . (document %? moveRight)
             }
    ]

actionMode :: Mode
actionMode = ActionMode "Action" $ standardActions ++ actions
