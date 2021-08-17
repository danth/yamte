module Yamte.Editor
  ( loadFile
  , reloadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  , handleEvent
  ) where

import Brick.Main (halt, continue)
import Brick.Types (EventM, BrickEvent(VtyEvent))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, try)
import Graphics.Vty (Event(EvKey))
import System.IO.Error (isDoesNotExistError)
import Yamte.Buffer
import Yamte.Types (Buffer(..), Event', EventM', Mode(..), ModeResponse(..), State(..), ModifiedKey)

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
activeMode state =
  case stateModes state of
    [] -> Nothing
    mode:modes -> Just mode

enterMode :: Mode -> State -> State
enterMode mode state = state {stateModes = mode : (stateModes state)}

leaveMode :: State -> State
leaveMode state = state {stateModes = tail $ stateModes state}

handleKey :: State -> [Mode] -> ModifiedKey -> IO State
handleKey state [] key = return state
handleKey state (mode:modes) key = do
  let (Mode _ f) = mode
  response <- f key state
  case response of
    NewState state' -> return state'
    Propagate -> handleKey state modes key
    DoNothing -> return state

handleEvent :: State -> Event' -> EventM'
handleEvent state (VtyEvent (EvKey key modifiers)) = do
  state' <- liftIO $ handleKey state (stateModes state) (key, modifiers)
  case stateModes state' of
    [] -> halt state'
    _ -> continue state'
handleEvent state _ = continue state
