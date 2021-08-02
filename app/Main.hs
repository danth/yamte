module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import UI.NCurses (runCurses, setEcho, Curses)
import Yamte.Display (createDisplayState, draw, getEvent, DisplayState)
import Yamte.Editor (initialState, loadFile, activeMode, enterMode, handleEvent, State)
import Yamte.Mode.Action (actionMode)

eventLoop :: DisplayState -> State -> Curses ()
eventLoop displayState state = do
    displayState' <- draw displayState state
    event <- getEvent displayState
    case event of
      Nothing -> eventLoop displayState' state
      Just e -> let state' = handleEvent e state
                 in case activeMode state' of
                      Nothing -> return ()
                      Just _ -> eventLoop displayState' state'

main :: IO ()
main = runCurses $ do
    setEcho False

    displayState <- createDisplayState

    arguments <- liftIO getArgs
    state <- if length arguments > 0
                 then liftIO $ loadFile (arguments!!0) initialState
                 else return initialState

    let state' = enterMode actionMode state

    eventLoop displayState state'
