module Yamte.Mode.Action (actionMode) where

import Yamte.Editor (Mode(Mode), Action(Action), leaveMode)

actionMode :: Mode
actionMode = Mode "Action" [ (Action (Left '\^Q') leaveMode)
                           ]
