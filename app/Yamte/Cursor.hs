module Yamte.Cursor (
  moveLeft,
  moveRight,
  moveUp,
  moveDown
) where

import Yamte.Editor (State(..), Buffer, Cursor)

clamp :: Ord a => a -> a -> a -> a
clamp min max value = if value < min
                         then min
                         else if value > max
                                  then max
                                  else value

clampCursor :: Buffer -> Cursor -> Cursor
clampCursor buffer (row, column) =
  let row' = clamp 0 (length buffer) row
  -- The cursor is allowed to move one row below the end of the buffer
   in if row' < length buffer
      then let line = buffer !! row'
               column' = clamp 0 (length line) column
            in (row', column')
      else (row', 0)

moveBy :: (Int, Int) -> State -> State
moveBy (rowOffset, columnOffset) state =
  let (row, column) = stateCursor state
      row' = row + rowOffset
      column' = column + columnOffset
      cursor = clampCursor (stateBuffer state) (row', column')
   in state { stateCursor = cursor }

moveLeft :: State -> State
moveLeft = moveBy (0, -1)

moveRight :: State -> State
moveRight = moveBy (0, 1)

moveUp :: State -> State
moveUp = moveBy (-1, 0)

moveDown :: State -> State
moveDown = moveBy (1, 0)
