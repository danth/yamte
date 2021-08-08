module Yamte.Mode.GenericAction
  ( Action(..)
  , makeActionMode
  ) where

import Data.List (find, intercalate)
import Yamte.Editor (Mode(Mode), ModeResponse(..), State(..), Trigger)
import UI.NCurses (Key(..))

data Action
  = Action Trigger (State -> State)
  | IOAction Trigger (State -> IO State)

showKey :: Key -> String
showKey KeyUpArrow = "↑"
showKey KeyDownArrow = "↓"
showKey KeyLeftArrow = "←"
showKey KeyRightArrow = "→"
showKey key = drop 3 $ show key

instance Show Action where
  show (Action (Left char) _) = [char]
  show (Action (Right key) _) = showKey key
  show (IOAction (Left char) _) = [char]
  show (IOAction (Right key) _) = showKey key

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

showHints :: [Action] -> State -> State
showHints actions state =
  let message = intercalate " " $ map show actions
   in state {stateMessage = message}

makeHintAction :: [Action] -> Action
makeHintAction actions = Action (Left '?') $ showHints actions

makeActionMode :: String -> [Action] -> Mode
makeActionMode name actions =
  let actions' = (makeHintAction actions):actions
   in Mode name $ handleTrigger actions'
