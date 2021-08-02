module Yamte.Mode.Action (actionMode) where

import Data.List (find)
import Yamte.Editor
import Yamte.Cursor
import Yamte.Mode.Input (inputMode)
import UI.NCurses (Key(..))

data Action = Action Trigger (State -> State)
            | IOAction Trigger (State -> IO State)

actions = [ (Action (Left '\^Q') leaveMode)
          , (Action (Left 'a') moveLeft)
          , (Action (Right KeyLeftArrow) moveLeft)
          , (Action (Left 'd') moveRight)
          , (Action (Right KeyRightArrow) moveRight)
          , (Action (Left 'w') moveUp)
          , (Action (Right KeyUpArrow) moveUp)
          , (Action (Left 's') moveDown)
          , (Action (Right KeyDownArrow) moveDown)
          , (Action (Left '\^W') moveTop)
          , (Action (Left '\^S') moveBottom)
          , (Action (Left '\^A') moveHome)
          , (Action (Right KeyHome) moveHome)
          , (Action (Left '\^D') moveEnd)
          , (Action (Right KeyEnd) moveEnd)
          , (Action (Left 'e') $ enterMode inputMode)
          , (IOAction (Left '\^O') saveFile)
          ]

getTrigger :: Action -> Trigger
getTrigger (Action trigger _) = trigger
getTrigger (IOAction trigger _) = trigger

getAction :: Trigger -> Maybe Action
getAction trigger = find (\action -> getTrigger action == trigger) actions

handleTrigger :: Trigger -> State -> IO ModeResponse
handleTrigger trigger state =
    case getAction trigger of
      Nothing -> return Propagate
      Just (Action _ f) -> return $ NewState $ f state
      Just (IOAction _ f) -> do
        state' <- f state
        return $ NewState state'

actionMode :: Mode
actionMode = Mode "Action" handleTrigger
