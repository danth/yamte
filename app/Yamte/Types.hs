module Yamte.Types
  ( Cursor
  , BufferText
  , Buffer(..)
  , ModifiedKey(..)
  , Action(..)
  , Mode(..)
  , ModeResponse(..)
  , State(..)
  , Event
  , Event'
  , Resource(..)
  , Widget'
  , EventM'
  ) where

import Brick.Types (BrickEvent, EventM, Next, Widget)
import Data.Default.Class (Default(..))
import Data.List (intercalate)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Graphics.Vty (Key(KChar, KDown, KFun, KLeft, KRight, KUp), Modifier(..))
import Skylighting (syntaxByName)
import Skylighting.Syntax (defaultSyntaxMap)
import Skylighting.Types (SourceLine, Syntax)

instance Default Syntax where
  def =
    case syntaxByName defaultSyntaxMap (T.pack "Default") of
      Nothing -> error "Default syntax does not exist"
      Just syntax -> syntax

type Cursor = (Int, Int)

type BufferText = S.Seq T.Text

data Buffer =
  Buffer
    { bufferText :: BufferText
    , bufferHighlighted :: [SourceLine]
    , bufferSyntax :: Syntax
    , bufferTouched :: Bool
    , bufferFilename :: Maybe String
    }

instance Default Buffer where
  def =
    Buffer
      { bufferText = S.singleton T.empty
      , bufferHighlighted = [[]]
      , bufferSyntax = def
      , bufferTouched = False
      , bufferFilename = Nothing
      }

data ModifiedKey = ModifiedKey Key [Modifier] deriving (Eq)

showKey :: Key -> String
showKey KDown = "↓"
showKey KLeft = "←"
showKey KRight = "→"
showKey KUp = "↑"
showKey (KChar char) = [char]
showKey (KFun num) = "F" ++ show num
showKey key = tail $ show key

showModifier :: Modifier -> String
showModifier MShift = "Shift"
showModifier MCtrl = "Ctrl"
showModifier MMeta = "Meta"
showModifier MAlt = "Alt"

instance Show ModifiedKey where
  show (ModifiedKey key modifiers) =
    intercalate "+" $ map showModifier modifiers ++ [showKey key]

data Action
  = Action ModifiedKey (State -> State)
  | IOAction ModifiedKey (State -> IO State)

data Mode
  = FunctionMode String (ModifiedKey -> State -> IO ModeResponse)
  | ActionMode String [Action]

instance Show Mode where
  show (FunctionMode name _) = name
  show (ActionMode name _) = name

data ModeResponse
  = NewState State
  | Propagate
  | DoNothing

data State =
  State
    { stateBuffer :: Buffer
    , stateMessage :: String
    , stateModes :: [Mode]
    , stateCursor :: Cursor
    }

instance Default State where
  def =
    State
      { stateBuffer = def
      , stateMessage = "Welcome to Yamte!"
      , stateModes = []
      , stateCursor = (0, 0)
      }

type Event = ()

type Event' = BrickEvent Resource Event

data Resource =
  FileCursor
  deriving (Show, Eq, Ord)

type Widget' = Widget Resource

type EventM' = EventM Resource (Next State)
