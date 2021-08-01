module Yamte.Mode.Input (inputMode) where

import Yamte.Editor
import UI.NCurses (Key(..))

handleTrigger :: Trigger -> State -> ModeResponse
handleTrigger (Right _) state = Propagate
handleTrigger (Left '\^Q') state = Propagate
handleTrigger (Left _) state = DoNothing

inputMode :: Mode
inputMode = Mode "Input" handleTrigger
