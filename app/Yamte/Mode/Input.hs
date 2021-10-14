module Yamte.Mode.Input ( inputMode ) where

import Data.Char ( isPrint )
import Data.Tree ( Tree(rootLabel) )
import Data.Tree.Cursor ( selection )

import Graphics.Vty ( Key(KBS, KChar, KDel, KEnter) )

import Lens.Micro ( (%~), (^.), to )

import Text.Parsec ( parse )

import Yamte.AST ( stringifyAST )
import Yamte.Types
  ( AST
  , Mode(FunctionMode)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State
  , document
  , parser
  )

parseText :: AST -> String -> AST
parseText originalAST text = case parse (originalAST ^. to rootLabel . parser)
  "" text of Left _ -> originalAST
             Right newAST -> newAST

modifyText :: (String -> String) -> State -> State
modifyText f = document . selection
  %~ (\ast -> parseText ast (f $ stringifyAST ast))

backspace :: State -> State
backspace = modifyText init

insertText :: String -> State -> State
insertText text = modifyText (++ text)

insertCharacter :: Char -> State -> State
insertCharacter character = insertText [ character ]

handleTrigger :: ModifiedKey -> State -> ModeResponse
handleTrigger (ModifiedKey KBS []) = NewState . backspace
handleTrigger (ModifiedKey (KChar '\127') []) = NewState . backspace
handleTrigger (ModifiedKey KDel []) = NewState . backspace
handleTrigger (ModifiedKey KEnter []) = NewState . insertCharacter '\n'
handleTrigger (ModifiedKey (KChar '\t') []) = NewState . insertText "    "
handleTrigger (ModifiedKey (KChar character) [])
  | isPrint character = NewState . insertCharacter character
  | otherwise = const DoNothing
handleTrigger _ = const Propagate

handleTrigger' :: ModifiedKey -> State -> IO ModeResponse
handleTrigger' trigger state = return $ handleTrigger trigger state

inputMode :: Mode
inputMode = FunctionMode "Input" handleTrigger'
