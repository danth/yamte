module Yamte.Cursor
  ( moveLeft
  , moveRight
  , moveUp
  , moveDown
  , moveTop
  , moveBottom
  , moveHome
  , moveEnd
  , moveRow
  , moveColumn
  ) where

import qualified Data.Sequence as S
import qualified Data.Text as T
import Yamte.Types (Buffer(bufferText), BufferText, Cursor, State(..))

rowLength :: BufferText -> Int -> Int
rowLength buffer row = T.length $ buffer `S.index` row

rowLength' :: State -> Int -> Int
rowLength' state = rowLength $ bufferText $ stateBuffer state

clamp :: Ord a => a -> a -> a -> a
clamp min max value
  | value < min = min
  | value > max = max
  | otherwise = value

clampCursor :: BufferText -> Cursor -> Cursor
clampCursor buffer (row, column) =
  let row' = clamp 0 ((length buffer) - 1) row
      column' = clamp 0 (rowLength buffer row') column
   in (row', column')

clampCursor' :: State -> Cursor -> Cursor
clampCursor' state = clampCursor $ bufferText $ stateBuffer state

move :: State -> (Cursor -> Cursor) -> State
move state f =
  let newCursor = f $ stateCursor state
      newCursor' = clampCursor' state newCursor
   in state {stateCursor = newCursor'}

moveLeft :: State -> State
moveLeft state =
  move state $ \(row, column) ->
    if column > 0
      then (row, column - 1)
      else (row - 1, maxBound)

moveRight :: State -> State
moveRight state =
  move state $ \(row, column) ->
    let columns = rowLength' state row
     in if column < columns
          then (row, column + 1)
          else (row + 1, 0)

moveUp :: State -> State
moveUp state = move state $ \(row, column) -> (row - 1, column)

moveDown :: State -> State
moveDown state = move state $ \(row, column) -> (row + 1, column)

moveTop :: State -> State
moveTop state = move state $ \(row, column) -> (0, column)

moveBottom :: State -> State
moveBottom state = move state $ \(row, column) -> (maxBound, column)

moveHome :: State -> State
moveHome state = move state $ \(row, column) -> (row, 0)

moveEnd :: State -> State
moveEnd state = move state $ \(row, column) -> (row, maxBound)

moveRow :: Int -> State -> State
moveRow row' state = move state $ \(row, column) -> (row', column)

moveColumn :: Int -> State -> State
moveColumn column' state = move state $ \(row, column) -> (row, column')
