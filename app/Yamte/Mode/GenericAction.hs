module Yamte.Mode.GenericAction
  ( makeActionMode
  ) where

import Data.List (find, intercalate)
import Graphics.Vty (Key(KUp, KDown, KLeft, KRight, KChar, KFun), Modifier(..))
import Yamte.Types (Action(..), Mode(..), ModeResponse(..), State(..), ModifiedKey)

showKey :: Key -> String
showKey KUp = "↑"
showKey KDown = "↓"
showKey KLeft = "←"
showKey KRight = "→"
showKey (KChar char) = [char]
showKey (KFun num) = "F" ++ show num
showKey key = tail $ show key

showModifier :: Modifier -> String
showModifier MShift = "Shift"
showModifier MCtrl = "Ctrl"
showModifier MMeta = "Meta"
showModifier MAlt = "Alt"

showModifiedKey :: ModifiedKey -> String
showModifiedKey (key, modifiers) =
  intercalate "+" $ (map showModifier modifiers) ++ [showKey key]

instance Show Action where
  show (Action key _) = showModifiedKey key
  show (IOAction key _) = showModifiedKey key

getTrigger :: Action -> ModifiedKey
getTrigger (Action trigger _) = trigger
getTrigger (IOAction trigger _) = trigger

findAction :: [Action] -> ModifiedKey -> Maybe Action
findAction actions trigger =
  find (\action -> getTrigger action == trigger) actions

handleTrigger :: [Action] -> ModifiedKey -> State -> IO ModeResponse
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
makeHintAction actions = Action (KChar '?', []) $ showHints actions

makeActionMode :: String -> [Action] -> Mode
makeActionMode name actions =
  let actions' = (makeHintAction actions) : actions
   in Mode name $ handleTrigger actions'
