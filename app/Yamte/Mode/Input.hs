module Yamte.Mode.Input
  ( inputMode
  ) where

import Data.Char (isPrint)
import qualified Data.Sequence as S
import qualified Data.Text as T
import UI.NCurses (Key(..))
import Yamte.Buffer (Buffer(..), BufferText, modifyBuffer)
import Yamte.Cursor (moveDown, moveHome, moveLeft, moveRight)
import Yamte.Editor

modifyState :: (BufferText -> BufferText) -> State -> State
modifyState f state = state {stateBuffer = modifyBuffer f $ stateBuffer state}

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

insertNewline' :: Cursor -> BufferText -> BufferText
insertNewline' (row, column) buffer =
  let line = buffer `S.index` row
      (front, back) = T.splitAt column line
   in S.insertAt (row + 1) back $ S.update row front buffer

insertNewline :: State -> State
insertNewline state =
  (moveHome . moveDown . (modifyState $ insertNewline' $ stateCursor state))
    state

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

handleTrigger :: Trigger -> State -> ModeResponse
handleTrigger (Right KeyBackspace) = NewState . backspace
handleTrigger (Right KeyDeleteCharacter) = NewState . backspace . moveRight
handleTrigger (Left '\n') = NewState . insertNewline
handleTrigger (Right _) = const Propagate
handleTrigger (Left '\^Q') = const Propagate
handleTrigger (Left character)
  | isPrint character = NewState . insertCharacter character
  | otherwise = const DoNothing

handleTrigger' :: Trigger -> State -> IO ModeResponse
handleTrigger' trigger state = return $ handleTrigger trigger state

inputMode :: Mode
inputMode = Mode "Input" handleTrigger'
