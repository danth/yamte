module Yamte.Display (
  DisplayState,
  createDisplayState,
  draw,
  Yamte.Display.getEvent
) where

import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Sequence as S
import qualified Data.Text as T
import UI.NCurses
import Yamte.Editor (Mode(Mode), State(..), activeMode)

type View = (Int, Int)
data DisplayState  = DisplayState { statusWindow :: Window
                                  , sidebarWindow :: Window
                                  , bufferWindow :: Window
                                  , messageWindow :: Window
                                  , view :: View
                                  }

invertBackground :: Update ()
invertBackground = setBackground $ Glyph ' ' [AttributeReverse]

createDisplayState :: Curses DisplayState
createDisplayState = do
    (rows, columns) <- screenSize

    status <- newWindow 1 columns 0 0
    updateWindow status invertBackground

    sidebar <- newWindow (rows-2) 4 1 0
    updateWindow sidebar invertBackground

    buffer <- newWindow (rows-2) (columns-4) 1 4

    message <- newWindow 1 columns (rows-1) 0
    updateWindow message invertBackground

    return DisplayState { statusWindow = status
                        , sidebarWindow = sidebar
                        , bufferWindow = buffer
                        , messageWindow = message
                        , view = (0, 0)
                        }

scrollAxis :: Int -> Int -> Int -> Int
scrollAxis cursorPosition scrollOffset displaySize
  | cursorPosition < scrollOffset = cursorPosition
  | cursorPosition >= scrollOffset + displaySize = cursorPosition - displaySize + 1
  | otherwise = scrollOffset

scroll :: DisplayState -> State -> Curses DisplayState
scroll displayState state = updateWindow (bufferWindow displayState) $ do
    let (row, column) = stateCursor state
        (rowOffset, columnOffset) = view displayState
    (rows, columns) <- windowSize
    let view' = ( scrollAxis row rowOffset (fromIntegral rows)
                , scrollAxis column columnOffset (fromIntegral columns)
                )
    return displayState { view = view' }

statusLine :: [String] -> String
statusLine = intercalate " • "

draw' :: DisplayState -> State -> Curses ()
draw' displayState state = do
    let (rowOffset, columnOffset) = view displayState

    updateWindow (statusWindow displayState) $ do
        clear
        moveCursor 0 4
        drawString $ statusLine
            [ (case stateFilename state of
                 Nothing -> "[No name]"
                 Just filename -> filename)
            , (show (length $ stateBuffer state) ++ " lines")
            , (case activeMode state of
                 Nothing -> "Not in a mode"
                 Just (Mode name _) -> name ++ " mode")
            ]

    (rows, columns) <- updateWindow (bufferWindow displayState) windowSize
    let scrolledBuffer :: S.Seq T.Text
        scrolledBuffer =
            S.take (fromIntegral rows)
            $ S.drop rowOffset
            $ stateBuffer state
        lines :: S.Seq (Int, Int, T.Text)
        lines = S.mapWithIndex (\i l -> (i, i + rowOffset + 1, l)) scrolledBuffer

    updateWindow (sidebarWindow displayState) $ do
        clear
        forM_ lines $ \(screenIndex, actualIndex, line) -> do
            if actualIndex < (length $ stateBuffer state)
            then do
                moveCursor (toInteger screenIndex) 0
                drawString $ show actualIndex
            else do
                moveCursor (toInteger screenIndex) 1
                drawString "~"

    updateWindow (bufferWindow displayState) $ do
        clear
        forM_ lines $ \(screenIndex, actualIndex, line) -> do
            moveCursor (toInteger screenIndex) 0
            drawText
                $ T.take (fromIntegral columns)
                $ T.drop columnOffset line

    updateWindow (messageWindow displayState) $ do
        clear
        moveCursor 0 4
        drawString $ stateMessage state

    updateWindow (bufferWindow displayState) $
        let (row, column) = stateCursor state
            row' = (fromIntegral row) - rowOffset
            column' = (fromIntegral column) - columnOffset
         in moveCursor (fromIntegral row') (fromIntegral column')

    render

draw :: DisplayState -> State -> Curses ()
draw displayState state = do
    displayState' <- scroll displayState state
    draw' displayState' state

getEvent :: DisplayState -> Curses (Maybe Event)
getEvent displayState = UI.NCurses.getEvent (bufferWindow displayState) Nothing
