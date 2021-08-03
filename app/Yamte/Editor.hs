module Yamte.Editor
  ( Trigger
  , ModeResponse(..)
  , Mode(..)
  , Buffer
  , Cursor
  , State(..)
  , initialState
  , loadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  , handleEvent
  ) where

import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Text as T
import UI.NCurses (Event(EventCharacter, EventSpecialKey), Key)

type Trigger = Either Char Key

data ModeResponse
  = NewState State
  | Propagate
  | DoNothing

data Mode =
  Mode String (Trigger -> State -> IO ModeResponse)

type Buffer = S.Seq T.Text

type Cursor = (Int, Int)

data State =
  State
    { stateBuffer :: Buffer
    , stateTouched :: Bool
    , stateFilename :: Maybe String
    , stateMessage :: String
    , stateModes :: [Mode]
    , stateCursor :: Cursor
    }

initialState :: State
initialState =
  State
    { stateBuffer = S.singleton T.empty
    , stateTouched = False
    , stateFilename = Nothing
    , stateMessage = "Welcome to Yamte!"
    , stateModes = []
    , stateCursor = (0, 0)
    }

loadFile :: String -> State -> IO State
loadFile filename state = do
  file <- readFile filename
  return $
    state
      { stateBuffer = S.fromList $ map T.pack $ lines file
      , stateFilename = Just filename
      , stateMessage = "Opened " ++ filename
      }

saveFile :: State -> IO State
saveFile state =
  case stateFilename state of
    Nothing -> return state
    Just filename -> do
      writeFile filename $ T.unpack $ T.unlines $ toList $ stateBuffer state
      return state {stateMessage = "Saved " ++ filename, stateTouched = False}

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
