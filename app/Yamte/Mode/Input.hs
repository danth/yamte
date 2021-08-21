module Yamte.Mode.Input ( inputMode ) where

import Data.Char ( isPrint )
import qualified Data.Sequence as S
import qualified Data.Text as T

import Graphics.Vty ( Key(KBS, KChar, KDel, KEnter) )

import Lens.Micro ( (%~), (&), (.~), (^.) )

import Yamte.Buffer ( text )
import Yamte.Cursor ( moveLeft, moveRight )
import Yamte.Types
  ( BufferText
  , Cursor
  , Mode(FunctionMode)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State
  , buffer
  , cursor
  )

modifyStateCursor
  :: (Cursor -> BufferText -> ( BufferText, Cursor )) -> State -> State
modifyStateCursor f state
  = let ( text', cursor' ) = f (state ^. cursor) (state ^. buffer . text)
    in state & buffer . text .~ text' & cursor .~ cursor'

deleteColumn :: Int -> T.Text -> T.Text
deleteColumn column line = let ( front, back ) = T.splitAt column line
                           in T.init front `T.append` back

deleteNewline :: Int -> BufferText -> BufferText
deleteNewline 0 buffer = buffer
deleteNewline row buffer
  = let line = buffer `S.index` row
    in S.deleteAt row $ S.adjust' (`T.append` line) (row - 1) buffer

backspace' :: Cursor -> State -> State
backspace' ( 0, 0 ) state = state
backspace' ( row, 0 ) state
  = let row' = row - 1
        line = (state ^. buffer . text) `S.index` row'
        column' = T.length line
    in state & cursor .~ ( row', column' ) & buffer . text %~ deleteNewline row
backspace' ( row, column ) state
  = state & buffer . text %~ S.adjust' (deleteColumn column) row & moveLeft

backspace :: State -> State
backspace state = backspace' (state ^. cursor) state

insertNewline :: State -> State
insertNewline state
  = let ( row, column ) = state ^. cursor
        line = (state ^. buffer . text) `S.index` row
        ( front, back ) = T.splitAt column line
        indentation = T.takeWhile (' ' ==) front
        newLine = indentation `T.append` back
    in state
       & buffer . text %~ S.insertAt (row + 1) newLine . S.update row front
       & cursor .~ ( row + 1, T.length indentation )

insertCharacter' :: Char -> Cursor -> BufferText -> BufferText
insertCharacter' character ( row, column ) buffer
  = let line = buffer `S.index` row
        ( front, back ) = T.splitAt column line
        line' = front `T.append` (character `T.cons` back)
    in S.update row line' buffer

insertCharacter :: Char -> State -> State
insertCharacter character state = state
  & buffer . text %~ insertCharacter' character (state ^. cursor)
  & moveRight

repeatCall :: Int -> (a -> a) -> (a -> a)
repeatCall 1 function = function
repeatCall count function = function . repeatCall (count - 1) function

handleTrigger :: ModifiedKey -> State -> ModeResponse
handleTrigger (ModifiedKey KBS []) = NewState . backspace
handleTrigger (ModifiedKey (KChar '\127') []) = NewState . backspace
handleTrigger (ModifiedKey KDel []) = NewState . backspace . moveRight
handleTrigger (ModifiedKey KEnter []) = NewState . insertNewline
handleTrigger (ModifiedKey (KChar '\t') []) = NewState
  . repeatCall 4 (insertCharacter ' ')
handleTrigger (ModifiedKey (KChar character) [])
  | isPrint character = NewState . insertCharacter character
  | otherwise = const DoNothing
handleTrigger _ = const Propagate

handleTrigger' :: ModifiedKey -> State -> IO ModeResponse
handleTrigger' trigger state = return $ handleTrigger trigger state

inputMode :: Mode
inputMode = FunctionMode "Input" handleTrigger'
