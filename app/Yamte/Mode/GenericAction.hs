module Yamte.Mode.GenericAction
  ( Action(..)
  , makeActionMode
  ) where

import Data.List (find)
import Yamte.Editor (Mode(Mode), ModeResponse(..), State, Trigger)

data Action
  = Action Trigger (State -> State)
  | IOAction Trigger (State -> IO State)

getTrigger :: Action -> Trigger
getTrigger (Action trigger _) = trigger
getTrigger (IOAction trigger _) = trigger

findAction :: [Action] -> Trigger -> Maybe Action
findAction actions trigger =
  find (\action -> getTrigger action == trigger) actions

handleTrigger :: [Action] -> Trigger -> State -> IO ModeResponse
handleTrigger actions trigger state =
  case findAction actions trigger of
    Nothing -> return Propagate
    Just (Action _ f) -> return $ NewState $ f state
    Just (IOAction _ f) -> do
      state' <- f state
      return $ NewState state'

makeActionMode :: String -> [Action] -> Mode
makeActionMode name actions = Mode name $ handleTrigger actions
