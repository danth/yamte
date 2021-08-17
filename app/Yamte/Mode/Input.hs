module Yamte.Mode.Input
  ( inputMode
  ) where

import Data.Char (isPrint)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Graphics.Vty (Key(KBS, KDel, KChar, KEnter), Modifier(MCtrl))
import Yamte.Types (Cursor, State(..), Mode(..), ModifiedKey, ModeResponse(..), Buffer(..), BufferText)
import Yamte.Buffer (modifyBuffer)
import Yamte.Cursor (moveDown, moveHome, moveLeft, moveRight)
import Yamte.Editor

modifyState :: (BufferText -> BufferText) -> State -> State
modifyState f state = state {stateBuffer = modifyBuffer f $ stateBuffer state}

modifyStateCursor ::
     (Cursor -> BufferText -> (BufferText, Cursor)) -> State -> State
modifyStateCursor f state =
  let f' = f $ stateCursor state
   in state
        { stateBuffer = modifyBuffer (fst . f') $ stateBuffer state
        , stateCursor = snd $ f' $ bufferText $ stateBuffer state
        }

deleteColumn :: Int -> T.Text -> T.Text
deleteColumn column line =
  let (front, back) = T.splitAt column line
   in (T.init front) `T.append` back

deleteNewline :: Int -> BufferText -> BufferText
deleteNewline 0 buffer = buffer
deleteNewline row buffer =
  let line = buffer `S.index` row
   in S.deleteAt row $ S.adjust' (\l -> l `T.append` line) (row - 1) buffer

backspace' :: Cursor -> State -> State
backspace' (0, 0) state = state
backspace' (row, 0) state =
  let buffer = stateBuffer state
      row' = row - 1
      line = (bufferText buffer) `S.index` row'
      column' = T.length line
   in state
        { stateCursor = (row', column')
        , stateBuffer = modifyBuffer (deleteNewline row) buffer
        }
backspace' (row, column) state =
  (moveLeft . (modifyState $ S.adjust' (deleteColumn column) row)) state

backspace :: State -> State
backspace state = backspace' (stateCursor state) state

insertNewline' :: Cursor -> BufferText -> (BufferText, Cursor)
insertNewline' (row, column) buffer =
  let line = buffer `S.index` row
      (front, back) = T.splitAt column line
      indentation = T.takeWhile ((==) ' ') front
      newLine = indentation `T.append` back
   in ( S.insertAt (row + 1) newLine $ S.update row front buffer
      , (row + 1, T.length indentation))

insertNewline :: State -> State
insertNewline = modifyStateCursor insertNewline'

insertCharacter' :: Char -> Cursor -> BufferText -> BufferText
insertCharacter' character (row, column) buffer =
  let line = buffer `S.index` row
      (front, back) = T.splitAt column line
      line' = front `T.append` (character `T.cons` back)
   in S.update row line' buffer

insertCharacter :: Char -> State -> State
insertCharacter character state =
  (moveRight . (modifyState $ insertCharacter' character $ stateCursor state))
    state

repeatCall :: Int -> (a -> a) -> (a -> a)
repeatCall 1 function = function
repeatCall count function = function . repeatCall (count - 1) function

handleTrigger :: ModifiedKey -> State -> ModeResponse
handleTrigger (KBS, []) = NewState . backspace
handleTrigger (KChar '\127', []) = NewState . backspace
handleTrigger (KDel, []) = NewState . backspace . moveRight
handleTrigger (KEnter, []) = NewState . insertNewline
handleTrigger (KChar '\t', []) = NewState . repeatCall 4 (insertCharacter ' ')
handleTrigger (KChar character, [])
  | isPrint character = NewState . insertCharacter character
  | otherwise = const DoNothing
handleTrigger _ = const Propagate

handleTrigger' :: ModifiedKey -> State -> IO ModeResponse
handleTrigger' trigger state = return $ handleTrigger trigger state

inputMode :: Mode
inputMode = Mode "Input" handleTrigger'
