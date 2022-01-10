module Yamte.Mode.Input ( inputMode, InputType(..), beginInput ) where

import Data.Char ( isPrint )
import Data.Tree ( Tree(rootLabel) )
import Data.Tree.Cursor ( selection )

import Graphics.Vty ( Key(KBS, KChar, KDel, KEnter), Modifier(MCtrl) )

import Lens.Micro ( (%~), (&), (^.), (.~), (?~), _Just, to )

import Text.Parsec ( parse )

import Yamte.AST ( stringifyAST )
import Yamte.Mode ( enterMode, leaveMode )
import Yamte.Types
  ( AST
  , Mode(FunctionMode)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State
  , currentInput
  , document
  , parser
  )

backspace :: State -> State
backspace = currentInput . _Just %~ init

insertText :: String -> State -> State
insertText text = currentInput . _Just %~ (++ text)

insertCharacter :: Char -> State -> State
insertCharacter character = insertText [ character ]

parseText :: String -> AST -> AST
parseText text originalAST = case parse (originalAST ^. to rootLabel . parser)
  "" text of Left _ -> originalAST
             Right newAST -> newAST

finaliseInput :: State -> State
finaliseInput state = case state ^. currentInput of
  Nothing -> state
  Just input -> state
    & document . selection %~ parseText input
    & currentInput .~ Nothing

handleTrigger :: ModifiedKey -> State -> ModeResponse
handleTrigger (ModifiedKey KBS []) = NewState . backspace
handleTrigger (ModifiedKey (KChar '\127') []) = NewState . backspace
handleTrigger (ModifiedKey KDel []) = NewState . backspace
handleTrigger (ModifiedKey KEnter []) = NewState . insertCharacter '\n'
handleTrigger (ModifiedKey (KChar '\t') []) = NewState . insertText "    "
handleTrigger (ModifiedKey (KChar 'q') [ MCtrl ])
  = NewState . leaveMode . finaliseInput
handleTrigger (ModifiedKey (KChar character) [])
  | isPrint character = NewState . insertCharacter character
  | otherwise = const DoNothing
handleTrigger _ = const Propagate

handleTrigger' :: ModifiedKey -> State -> IO ModeResponse
handleTrigger' trigger state = return $ handleTrigger trigger state

inputMode :: Mode
inputMode = FunctionMode "Input" handleTrigger'

data InputType = Edit | Overwrite

beginInput :: InputType -> State -> State
beginInput Edit state = state & currentInput ?~ input & enterMode inputMode
  where input = stringifyAST $ state ^. document . selection
beginInput Overwrite state = state & currentInput ?~ "" & enterMode inputMode
