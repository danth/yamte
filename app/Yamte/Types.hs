{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  , AST
  , State
  , document
  , filename
  , message
  , modes
  , showHints
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
import Data.Tree ( Tree(..) )

import Graphics.Vty
  ( Key(KBackTab, KChar, KDown, KFun, KLeft, KRight, KUp)
  , Modifier(..)
  )

import Lens.Micro.TH ( makeLenses )

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
  }

type AST = Tree SyntaxConstruct

data State = State
  { _document :: AST
  , _filename :: Maybe String
  , _message :: String
  , _modes :: [ Mode ]
  , _showHints :: Bool
  , _touched :: Bool
  }

instance Default State where
  def = State
    { _document = Node (SyntaxConstruct { _render = const W.emptyWidget
                                        , _stringify = const ""
                                        }) []
    , _filename = Nothing
    , _message = "Welcome to Yamte!"
    , _modes = []
    , _showHints = False
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
