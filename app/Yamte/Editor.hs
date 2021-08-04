module Yamte.Editor
  ( Trigger
  , ModeResponse(..)
  , Mode(..)
  , Cursor
  , State(..)
  , initialState
  , loadFile
  , reloadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  , handleEvent
  ) where

import UI.NCurses (Event(EventCharacter, EventSpecialKey), Key)
import Yamte.Buffer

type Trigger = Either Char Key

data ModeResponse
  = NewState State
  | Propagate
  | DoNothing

data Mode =
  Mode String (Trigger -> State -> IO ModeResponse)

type Cursor = (Int, Int)

data State =
  State
    { stateBuffer :: Buffer
    , stateMessage :: String
    , stateModes :: [Mode]
    , stateCursor :: Cursor
    }

initialState :: State
initialState =
  State
    { stateBuffer = emptyBuffer
    , stateMessage = "Welcome to Yamte!"
    , stateModes = []
    , stateCursor = (0, 0)
    }

loadFile :: String -> State -> IO State
loadFile filename state = do
  buffer <- bufferFromFile filename
  return $ state {stateBuffer = buffer, stateMessage = "Opened " ++ filename}

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

getTrigger :: Event -> Maybe Trigger
getTrigger (EventCharacter character) = Just $ Left character
getTrigger (EventSpecialKey key) = Just $ Right key
getTrigger _ = Nothing

handleTrigger :: Trigger -> State -> [Mode] -> IO State
handleTrigger trigger state [] = return state
handleTrigger trigger state (mode:modes) = do
  let (Mode _ f) = mode
  response <- f trigger state
  case response of
    NewState state' -> return state'
    Propagate -> handleTrigger trigger state modes
    DoNothing -> return state

handleEvent :: Event -> State -> IO State
handleEvent event state =
  case getTrigger event of
    Nothing -> return state
    Just trigger -> handleTrigger trigger state (stateModes state)
