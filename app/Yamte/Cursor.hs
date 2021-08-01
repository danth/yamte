module Yamte.Cursor (
  moveLeft,
  moveRight,
  moveUp,
  moveDown,
  moveTop,
  moveBottom,
  moveHome,
  moveEnd
) where

import qualified Data.Sequence as S
import Yamte.Editor (State(..), Buffer, Cursor)

rowLength :: Buffer -> Int -> Int
-- The cursor is allowed to move one row below the end of the buffer
rowLength buffer row = if row < length buffer
                          then length $ buffer `S.index` row
                          else 0

rowLength' :: State -> Int -> Int
rowLength' state = rowLength $ stateBuffer state

clamp :: Ord a => a -> a -> a -> a
clamp min max value = if value < min
                         then min
                         else if value > max
                                  then max
                                  else value

clampCursor :: Buffer -> Cursor -> Cursor
clampCursor buffer (row, column) =
  let row' = clamp 0 (length buffer) row
      column' = clamp 0 (rowLength buffer row') column
   in (row', column')

clampCursor' :: State -> Cursor -> Cursor
clampCursor' state = clampCursor $ stateBuffer state

move :: State -> (Cursor -> Cursor) -> State
move state f =
  let newCursor = f $ stateCursor state
      newCursor' = clampCursor' state newCursor
   in state { stateCursor = newCursor' }

moveLeft :: State -> State
moveLeft state = move state $ \(row, column) ->
  if column > 0
     then (row, column - 1)
     else (row - 1, maxBound)

moveRight :: State -> State
moveRight state = move state $ \(row, column) ->
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
