{-# LANGUAGE TemplateHaskell #-}

module Yamte.Types
  ( ModifiedKey(..)
  , Action(..)
  , trigger
  , description
  , transformation
  , Mode(..)
  , ModeResponse(..)
  , SyntaxConstruct(..)
  , render
  , stringify
  , parser
  , AST
  , ASTCursor
  , State
  , currentInput
  , document
  , filename
  , message
  , modeStack
  , touched
  , Event
  , Event'
  , Resource(..)
  , Widget'
  , EventM'
  ) where

import Brick.Types ( BrickEvent, EventM, Next, Widget )
import qualified Brick.Widgets.Core as W

import Data.Default.Class ( Default(..) )
import Data.Stack ( Stack )
import Data.Tree ( Tree(..) )
import Data.Tree.Cursor ( TreeCursor, toCursor )

import Graphics.Vty
  ( Key(KBackTab, KChar, KDown, KFun, KLeft, KRight, KUp)
  , Modifier(..)
  )

import Lens.Micro.TH ( makeLenses )

import Text.Parsec ( Parsec )

data ModifiedKey = ModifiedKey Key [ Modifier ]
  deriving ( Eq )

showKey :: Key -> String
showKey KDown = "↓"
showKey KLeft = "←"
showKey KRight = "→"
showKey KUp = "↑"
showKey (KChar '\t') = "⇥"
showKey KBackTab = "⇤"
showKey (KChar char) = [ char ]
showKey (KFun num) = "F" ++ show num
showKey key = tail $ show key

showModifier :: Modifier -> String
showModifier MShift = "⇧"
showModifier MCtrl = "⌃"
showModifier MMeta = "◆"
showModifier MAlt = "⎇"

instance Show ModifiedKey where
  show (ModifiedKey key modifiers)
    = concatMap showModifier modifiers ++ showKey key

data Action = Action { _trigger :: ModifiedKey
                     , _description :: String
                     , _transformation :: State -> IO State
                     }

data Mode
  = FunctionMode String (ModifiedKey -> State -> IO ModeResponse)
  | ActionMode String [ Action ]

instance Show Mode where
  show (FunctionMode name _) = name
  show (ActionMode name _) = name

data ModeResponse = NewState State | Propagate | DoNothing

data SyntaxConstruct = SyntaxConstruct
  { _render :: [ Widget' ] -> Widget'
  , _stringify :: [ String ] -> String
  , _parser :: Parsec String () AST
  }

type AST = Tree SyntaxConstruct

type ASTCursor = TreeCursor SyntaxConstruct

data State = State
  { _currentInput :: Maybe String
  , _document :: ASTCursor
  , _filename :: Maybe String
  , _message :: String
  , _modeStack :: Stack Mode
  , _touched :: Bool
  }

instance Default State where
  def = State
    { _currentInput = Nothing
    , _document = toCursor
        $ Node (SyntaxConstruct { _render = const W.emptyWidget
                                , _stringify = const ""
                                , _parser = fail "Cannot modify empty document"
                                }) []
    , _filename = Nothing
    , _message = "Welcome to Yamte!"
    , _modeStack = mempty
    , _touched = False
    }

type Event = ()

type Event' = BrickEvent Resource Event

data Resource = MainViewport
  deriving ( Show, Eq, Ord )

type Widget' = Widget Resource

type EventM' = EventM Resource (Next State)

makeLenses ''Action

makeLenses ''SyntaxConstruct

makeLenses ''State
