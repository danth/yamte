module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import UI.NCurses (runCurses, setEcho, Curses)
import Yamte.Display (createWindows, draw, getEvent, Windows)
import Yamte.Editor (initialState, loadFile, activeMode, enterMode, handleEvent, State)
import Yamte.Mode.Action (actionMode)

eventLoop :: Windows -> State -> Curses ()
eventLoop windows state = do
    draw windows state
    event <- getEvent windows
    case event of
      Nothing -> eventLoop windows state
      Just e -> let state' = handleEvent e state
                 in case activeMode state' of
                      Nothing -> return ()
                      Just _ -> eventLoop windows state'

main :: IO ()
main = runCurses $ do
    setEcho False

    windows <- createWindows

    arguments <- liftIO getArgs
    state <- if length arguments > 0
                 then liftIO $ loadFile (arguments!!0) initialState
                 else return initialState

    let state' = enterMode actionMode state

    eventLoop windows state'
