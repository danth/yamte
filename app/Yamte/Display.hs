module Yamte.Display (
  Windows,
  createWindows,
  draw,
  Yamte.Display.getEvent
) where

import Control.Monad (forM_)
import Data.List (genericTake, intercalate)
import UI.NCurses
import Yamte.Editor (Mode(Mode), State(..), activeMode)

data Windows  = Windows { statusWindow :: Window
                        , sidebarWindow :: Window
                        , bufferWindow :: Window
                        , messageWindow :: Window
                        }

invertBackground :: Update ()
invertBackground = setBackground $ Glyph ' ' [AttributeReverse]

createWindows :: Curses Windows
createWindows = do
    (rows, columns) <- screenSize

    status <- newWindow 1 columns 0 0
    updateWindow status invertBackground

    sidebar <- newWindow (rows-2) 4 1 0
    updateWindow sidebar invertBackground

    buffer <- newWindow (rows-2) (columns-4) 1 4

    message <- newWindow 1 columns (rows-1) 0
    updateWindow message invertBackground

    return Windows { statusWindow = status
                   , sidebarWindow = sidebar
                   , bufferWindow = buffer
                   , messageWindow = message
                   }

statusLine :: [String] -> String
statusLine = intercalate " • "

draw :: Windows -> State -> Curses ()
draw windows state = do
    updateWindow (statusWindow windows) $ do
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

    updateWindow (sidebarWindow windows) $ do
        clear
        (rows, columns) <- windowSize
        forM_ [0..(rows-1)] $ \i ->
            if i < (toInteger $ length $ stateBuffer state)
            then do
                moveCursor i 0
                drawString $ show i
            else do
                moveCursor i 1
                drawString "~"

    updateWindow (bufferWindow windows) $ do
        clear
        (rows, columns) <- windowSize
        forM_ (zip [0..(rows-1)] $ stateBuffer state) $ \(i, line) -> do
            moveCursor i 0
            drawString $ genericTake (columns-1) line

    updateWindow (messageWindow windows) $ do
        clear
        moveCursor 0 4
        drawString $ stateMessage state

    updateWindow (bufferWindow windows) $
        let (row, column) = stateCursor state
         in moveCursor (fromIntegral row) (fromIntegral column)

    render

getEvent :: Windows -> Curses (Maybe Event)
getEvent windows = UI.NCurses.getEvent (bufferWindow windows) Nothing
