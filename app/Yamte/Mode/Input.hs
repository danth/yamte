module Yamte.Mode.Input (inputMode) where

import Data.Char (isPrint)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Yamte.Editor
import Yamte.Cursor (moveLeft, moveRight, moveHome, moveDown)
import UI.NCurses (Key(..))

deleteColumn :: Int -> T.Text -> T.Text
deleteColumn column line =
  let (front, back) = T.splitAt column line
   in (T.init front) `T.append` back

deleteNewline :: Int -> Buffer -> Buffer
deleteNewline 0 buffer = buffer
deleteNewline row buffer =
  let line = buffer `S.index` row
   in S.deleteAt row $ S.adjust' (\l -> l `T.append` line) (row-1) buffer

backspace' :: Cursor -> State -> State
backspace' (0, 0) state = state
backspace' (row, 0) state =
  let buffer = stateBuffer state
      row' = row - 1
   in state { stateCursor = (row', T.length $ buffer `S.index` row')
            , stateBuffer = deleteNewline row buffer
            }
backspace' (row, column) state = moveLeft $ state
  { stateBuffer = S.adjust' (deleteColumn column) row (stateBuffer state) }

backspace :: State -> State
backspace state = backspace' (stateCursor state) state

insertNewline' :: Cursor -> Buffer -> Buffer
insertNewline' (row, column) buffer =
  let line = buffer `S.index` row
      (front, back) = T.splitAt column line
   in S.insertAt (row + 1) back $ S.update row front buffer

insertNewline :: State -> State
insertNewline state = moveHome $ moveDown $ state
  { stateBuffer = insertNewline' (stateCursor state) (stateBuffer state) }

insertCharacter' :: Char -> Cursor -> Buffer -> Buffer
insertCharacter' character (row, column) buffer =
  let line = buffer `S.index` row
      (front, back) = T.splitAt column line
      line' = front `T.append` (character `T.cons` back)
   in S.update row line' buffer

insertCharacter :: Char -> State -> State
insertCharacter character state = moveRight $ state
  { stateBuffer = insertCharacter' character (stateCursor state) (stateBuffer state) }

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
