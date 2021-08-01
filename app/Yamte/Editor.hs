module Yamte.Editor (
  Trigger,
  Action(..),
  Mode(..),
  Buffer,
  Cursor,
  State(..),
  initialState,
  loadFile,
  activeMode,
  enterMode,
  leaveMode,
  handleEvent
) where

import Data.List (find)
import UI.NCurses (Key, Event(EventCharacter, EventSpecialKey))

type Trigger = Either Char Key
data Action = Action Trigger (State -> State)
data Mode = Mode String [Action]

type Buffer = [String]
type Cursor = (Int, Int)
data State = State { stateBuffer :: Buffer
                   , stateFilename :: Maybe String
                   , stateMessage :: String
                   , stateModes :: [Mode]
                   , stateCursor :: Cursor
                   }

initialState :: State
initialState = State { stateBuffer = []
                     , stateFilename = Nothing
                     , stateMessage = "Welcome to Yamte!"
                     , stateModes = []
                     , stateCursor = (0, 0)
                     }

loadFile :: String -> State -> IO State
loadFile filename state = do
    file <- readFile filename
    return $ state { stateBuffer = lines file
                   , stateFilename = Just filename
                   , stateMessage = "Opened " ++ filename
                   }

activeMode :: State -> Maybe Mode
activeMode state = case stateModes state of
                     [] -> Nothing
                     mode:modes -> Just mode

enterMode :: Mode -> State -> State
enterMode mode state = state { stateModes = mode:(stateModes state) }

leaveMode :: State -> State
leaveMode state = state { stateModes = tail $ stateModes state }

getAction :: Event -> State -> Maybe Action
getAction event state =
    let trigger :: Trigger
        trigger = case event of
                    EventCharacter character -> Left character
                    EventSpecialKey key -> Right key
     in do
         (Mode _ actions) <- activeMode state
         find (\(Action t _) -> t == trigger) actions

handleEvent :: Event -> State -> State
handleEvent event state = case getAction event state of
          Nothing -> state
          Just (Action _ f) -> f state
