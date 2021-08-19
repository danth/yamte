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
import qualified Data.Sequence as S
import qualified Data.Text as T
import Graphics.Vty
  ( Key(KBackTab, KChar, KDown, KFun, KLeft, KRight, KUp)
  , Modifier(..)
  )
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

data ModifiedKey =
  ModifiedKey Key [Modifier]
  deriving (Eq)

showKey :: Key -> String
showKey KDown = "↓"
showKey KLeft = "←"
showKey KRight = "→"
showKey KUp = "↑"
showKey (KChar '\t') = "⇥"
showKey KBackTab = "⇤"
showKey (KChar char) = [char]
showKey (KFun num) = "F" ++ show num
showKey key = tail $ show key

showModifier :: Modifier -> String
showModifier MShift = "⇧"
showModifier MCtrl = "⌃"
showModifier MMeta = "◆"
showModifier MAlt = "⎇"

instance Show ModifiedKey where
  show (ModifiedKey key modifiers) =
    concatMap showModifier modifiers ++ showKey key

data Action
  = Action ModifiedKey String (State -> State)
  | IOAction ModifiedKey String (State -> IO State)

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
    , stateShowHints :: Bool
    , stateCursor :: Cursor
    }

instance Default State where
  def =
    State
      { stateBuffer = def
      , stateMessage = "Welcome to Yamte!"
      , stateModes = []
      , stateShowHints = False
      , stateCursor = (0, 0)
      }

type Event = ()

type Event' = BrickEvent Resource Event

data Resource =
  FileCursor
  deriving (Show, Eq, Ord)

type Widget' = Widget Resource

type EventM' = EventM Resource (Next State)
