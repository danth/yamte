module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import UI.NCurses (Curses, runCurses, setEcho)
import Yamte.Display (DisplayState, createDisplayState, draw, getEvent)
import Yamte.Editor
  ( State
  , activeMode
  , enterMode
  , handleEvent
  , initialState
  , loadFile
  )
import Yamte.Mode.Action (actionMode)

eventLoop :: DisplayState -> State -> Curses ()
eventLoop displayState state = do
  displayState' <- draw displayState state
  event <- getEvent displayState
  case event of
    Nothing -> eventLoop displayState' state
    Just e -> do
      state' <- liftIO $ handleEvent e state
      case activeMode state' of
        Nothing -> return ()
        Just _ -> eventLoop displayState' state'

main :: IO ()
main =
  runCurses $ do
    setEcho False
    displayState <- createDisplayState
    arguments <- liftIO getArgs
    state <-
      if length arguments > 0
        then liftIO $ loadFile (arguments !! 0) initialState
        else return initialState
    let state' = enterMode actionMode state
    eventLoop displayState state'
