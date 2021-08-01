module Yamte.Editor (
  Trigger,
  ModeResponse(..),
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

import qualified Data.Sequence as S
import UI.NCurses (Key, Event(EventCharacter, EventSpecialKey))

type Trigger = Either Char Key
data ModeResponse = NewState State | Propagate | DoNothing
data Mode = Mode String (Trigger -> State -> ModeResponse)

type Buffer = S.Seq String
type Cursor = (Int, Int)
data State = State { stateBuffer :: Buffer
                   , stateFilename :: Maybe String
                   , stateMessage :: String
                   , stateModes :: [Mode]
                   , stateCursor :: Cursor
                   }

initialState :: State
initialState = State { stateBuffer = S.empty
                     , stateFilename = Nothing
                     , stateMessage = "Welcome to Yamte!"
                     , stateModes = []
                     , stateCursor = (0, 0)
                     }

loadFile :: String -> State -> IO State
loadFile filename state = do
    file <- readFile filename
    return $ state { stateBuffer = S.fromList $ lines file
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

getTrigger :: Event -> Maybe Trigger
getTrigger (EventCharacter character) = Just $ Left character
getTrigger (EventSpecialKey key) = Just $ Right key
getTrigger _ = Nothing

handleTrigger :: Trigger -> State -> [Mode] -> State
handleTrigger trigger state [] = state
handleTrigger trigger state (mode:modes) =
  let (Mode _ f) = mode
   in case f trigger state of
        NewState state' -> state'
        Propagate -> handleTrigger trigger state modes
        DoNothing -> state

handleEvent :: Event -> State -> State
handleEvent event state = case getTrigger event of
        Nothing -> state
        Just trigger -> handleTrigger trigger state (stateModes state)
