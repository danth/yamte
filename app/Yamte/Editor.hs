module Yamte.Editor
  ( loadFile
  , reloadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  , standardActions
  , handleEvent
  ) where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(VtyEvent), EventM)
import Control.Exception (Exception, try)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Graphics.Vty (Event(EvKey), Key(KChar), Modifier(MCtrl))
import System.IO.Error (isDoesNotExistError)
import Yamte.Buffer
import Yamte.Types
  ( Action(..)
  , Buffer(..)
  , Event'
  , EventM'
  , Mode(..)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State(..)
  )

loadFile' :: String -> State -> IO State
loadFile' filename state = do
  buffer <- bufferFromFile filename
  return $ state {stateBuffer = buffer, stateMessage = "Opened " ++ filename}

handleError :: IOError -> State -> State
handleError exception state
  | isDoesNotExistError exception =
    state {stateMessage = "File does not exist."}
  | otherwise = state {stateMessage = "Unknown error when loading file."}

loadFile :: String -> State -> IO State
loadFile filename state = do
  stateOrException <- try $ loadFile' filename state
  case stateOrException of
    Left exception -> return $ handleError exception state
    Right state' -> return state'

reloadFile :: State -> IO State
reloadFile state =
  case bufferFilename $ stateBuffer state of
    Nothing -> return state {stateMessage = "No file name specified"}
    Just filename -> loadFile filename state

saveFile :: State -> IO State
saveFile state =
  case bufferFilename $ stateBuffer state of
    Nothing -> return state {stateMessage = "No file name specified"}
    Just filename -> do
      buffer' <- bufferToFile $ stateBuffer state
      return state {stateBuffer = buffer', stateMessage = "Saved " ++ filename}

activeMode :: State -> Maybe Mode
activeMode = listToMaybe . stateModes

enterMode :: Mode -> State -> State
enterMode mode state = state {stateModes = mode : stateModes state}

leaveMode :: State -> State
leaveMode state = state {stateModes = tail $ stateModes state}

standardActions :: [Action]
standardActions = [hintAction, exitAction]
  where
    hintAction :: Action
    hintAction =
      Action (ModifiedKey (KChar '?') []) "Toggle hints" $ \state ->
        state {stateShowHints = not $ stateShowHints state}
    exitAction :: Action
    exitAction =
      Action (ModifiedKey (KChar 'q') [MCtrl]) "Exit this mode" leaveMode

getTrigger :: Action -> ModifiedKey
getTrigger (Action trigger _ _) = trigger
getTrigger (IOAction trigger _ _) = trigger

findAction :: [Action] -> ModifiedKey -> Maybe Action
findAction actions trigger =
  find (\action -> getTrigger action == trigger) actions

runAction :: Maybe Action -> State -> IO ModeResponse
runAction Nothing = const $ return DoNothing
runAction (Just (Action _ _ f)) = return . NewState . f
runAction (Just (IOAction _ _ f)) = f >=> return . NewState

handleKey :: Mode -> ModifiedKey -> State -> IO ModeResponse
handleKey (FunctionMode _ f) = f
handleKey (ActionMode _ actions) = runAction . findAction actions

handleKey' :: [Mode] -> ModifiedKey -> State -> IO State
handleKey' [] key state = return state
handleKey' (mode:modes) key state = do
  response <- handleKey mode key state
  case response of
    NewState state' -> return state'
    Propagate -> handleKey' modes key state
    DoNothing -> return state

handleEvent :: State -> Event' -> EventM'
handleEvent state (VtyEvent (EvKey key modifiers)) = do
  let key' = ModifiedKey key modifiers
  state' <- liftIO $ handleKey' (stateModes state) key' state
  case stateModes state' of
    [] -> halt state'
    _ -> continue state'
handleEvent state _ = continue state
