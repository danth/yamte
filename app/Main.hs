module Main where

import Brick.Main (App(..), defaultMain, showFirstCursor)
import Data.Default.Class (Default(def))
import System.Environment (getArgs)
import Yamte.Attributes (attributes)
import Yamte.Draw (draw)
import Yamte.Editor (enterMode, handleEvent, loadFile)
import Yamte.Mode.Action (actionMode)
import Yamte.Types (Event, Resource, State)

app :: App State Event Resource
app =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const attributes
    }

initialState :: State
initialState = enterMode actionMode def

main :: IO ()
main = do
  arguments <- getArgs
  state <-
    if length arguments > 0
      then loadFile (arguments !! 0) initialState
      else return initialState
  defaultMain app state
  return ()
