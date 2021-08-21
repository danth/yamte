{-# LANGUAGE TemplateHaskell #-}

module Yamte.Types
  ( Cursor
  , BufferText
  , Buffer
  , raw
  , highlighted
  , syntax
  , touched
  , filename
  , ModifiedKey(..)
  , Action(..)
  , Mode(..)
  , ModeResponse(..)
  , State
  , buffer
  , message
  , modes
  , showHints
  , cursor
  , Event
  , Event'
  , Resource(..)
  , Widget'
  , EventM'
  ) where

import Brick.Types ( BrickEvent, EventM, Next, Widget )

import Data.Default.Class ( Default(..) )
import qualified Data.Sequence as S
import qualified Data.Text as T

import Graphics.Vty
  ( Key(KBackTab, KChar, KDown, KFun, KLeft, KRight, KUp)
  , Modifier(..)
  )

import Lens.Micro.TH ( makeLenses )

import Skylighting ( syntaxByName )
import Skylighting.Syntax ( defaultSyntaxMap )
import Skylighting.Types ( SourceLine, Syntax )

instance Default Syntax where
  def = case syntaxByName defaultSyntaxMap (T.pack "Default") of
    Nothing -> error "Default syntax does not exist"
    Just syntax -> syntax

type Cursor = ( Int, Int )

type BufferText = S.Seq T.Text

data Buffer = Buffer
  { _raw :: BufferText
  , _highlighted :: [ SourceLine ]
  , _syntax :: Syntax
  , _touched :: Bool
  , _filename :: Maybe String
  }

instance Default Buffer where
  def = Buffer { _raw = S.singleton T.empty
               , _highlighted = [ [] ]
               , _syntax = def
               , _touched = False
               , _filename = Nothing
               }

makeLenses ''Buffer

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

data Action
  = Action ModifiedKey String (State -> State)
  | IOAction ModifiedKey String (State -> IO State)

data Mode
  = FunctionMode String (ModifiedKey -> State -> IO ModeResponse)
  | ActionMode String [ Action ]

instance Show Mode where
  show (FunctionMode name _) = name
  show (ActionMode name _) = name

data ModeResponse = NewState State | Propagate | DoNothing

data State = State { _buffer :: Buffer
                   , _message :: String
                   , _modes :: [ Mode ]
                   , _showHints :: Bool
                   , _cursor :: Cursor
                   }

instance Default State where
  def = State { _buffer = def
              , _message = "Welcome to Yamte!"
              , _modes = []
              , _showHints = False
              , _cursor = ( 0, 0 )
              }

makeLenses ''State

type Event = ()

type Event' = BrickEvent Resource Event

data Resource = FileCursor
  deriving ( Show, Eq, Ord )

type Widget' = Widget Resource

type EventM' = EventM Resource (Next State)
