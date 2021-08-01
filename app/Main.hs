module Main where

import UI.NCurses

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

draw :: Windows -> Curses ()
draw windows = do
    updateWindow (statusWindow windows) $ do
        clear

    updateWindow (sidebarWindow windows) $ do
        clear

    updateWindow (bufferWindow windows) $ do
        clear

    updateWindow (messageWindow windows) $ do
        clear
        moveCursor 0 0
        drawString "Welcome to Yamte!"

    updateWindow (bufferWindow windows) $ moveCursor 0 0

    render

main :: IO ()
main = runCurses $ do
    setEcho False

    windows <- createWindows
    draw windows

    waitFor (bufferWindow windows) (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
