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

import Yamte.Editor (State(..), Buffer, Cursor)

rowLength :: Int -> Buffer -> Int
-- The cursor is allowed to move one row below the end of the buffer
rowLength row buffer = if row < length buffer
                          then length $ buffer !! row
                          else 0

clamp :: Ord a => a -> a -> a -> a
clamp min max value = if value < min
                         then min
                         else if value > max
                                  then max
                                  else value

clampCursor :: Buffer -> Cursor -> Cursor
clampCursor buffer (row, column) =
  let row' = clamp 0 (length buffer) row
   in if row' < length buffer
      then let column' = clamp 0 (rowLength row' buffer) column
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

setCursor :: (Cursor -> Buffer -> Cursor) -> State -> State
setCursor f state =
  let newCursor = f (stateCursor state) (stateBuffer state)
   in state { stateCursor = clampCursor (stateBuffer state) newCursor }

moveTop :: State -> State
moveTop = setCursor (\(row, column) buffer -> (0, column))

moveBottom :: State -> State
moveBottom = setCursor (\(row, column) buffer -> (length buffer, column))

moveHome :: State -> State
moveHome = setCursor (\(row, column) buffer -> (row, 0))

moveEnd :: State -> State
moveEnd = setCursor (\(row, column) buffer -> (row, rowLength row buffer))
