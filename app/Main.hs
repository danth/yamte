module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (genericTake)
import System.Environment (getArgs)
import UI.NCurses

data Windows  = Windows { statusWindow :: Window
                        , sidebarWindow :: Window
                        , bufferWindow :: Window
                        , messageWindow :: Window
                        }

type Buffer = [String]

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

draw :: Windows -> Buffer -> Curses ()
draw windows buffer = do
    updateWindow (statusWindow windows) $ do
        clear

    updateWindow (sidebarWindow windows) $ do
        clear

    updateWindow (bufferWindow windows) $ do
        clear
        (rows, columns) <- windowSize
        forM_ (zip [0..(rows-1)] buffer) $ \(i, line) -> do
            moveCursor i 0
            drawString $ genericTake (columns-1) line

    updateWindow (messageWindow windows) $ do
        clear
        moveCursor 0 0
        drawString "Welcome to Yamte!"

    updateWindow (bufferWindow windows) $ moveCursor 0 0

    render

loadFile :: String -> IO Buffer
loadFile fileName = do
    file <- readFile fileName
    return $ lines file

main :: IO ()
main = runCurses $ do
    arguments <- liftIO getArgs
    buffer <- if length arguments > 0
                 then liftIO $ loadFile $ arguments!!0
                 else return []

    setEcho False

    windows <- createWindows
    draw windows buffer

    waitFor (bufferWindow windows) (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
