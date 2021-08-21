module Yamte.Editor
  ( loadFile
  , reloadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  ) where

import Control.Exception ( Exception, try )

import Data.Maybe ( listToMaybe )

import Lens.Micro ( (%~), (&), (.~), (^.) )

import System.IO.Error ( isDoesNotExistError )

import Yamte.Buffer
import Yamte.Types
  ( Buffer
  , Mode
  , State
  , buffer
  , filename
  , message
  , modes
  )

loadFile' :: String -> State -> IO State
loadFile' file state = do
  fileBuffer <- bufferFromFile file
  return $ state & buffer .~ fileBuffer & message .~ ("Opened " ++ file)

handleError :: IOError -> State -> State
handleError exception state
  | isDoesNotExistError exception = state & message .~ "File does not exist."
  | otherwise = state & message .~ "Unknown error when loading file."

loadFile :: String -> State -> IO State
loadFile filename state = do
  stateOrException <- try $ loadFile' filename state
  case stateOrException of
    Left exception -> return $ handleError exception state
    Right state' -> return state'

reloadFile :: State -> IO State
reloadFile state = case state ^. buffer . filename of
  Nothing -> return $ state & message .~ "No file name specified"
  Just filename -> loadFile filename state

saveFile :: State -> IO State
saveFile state = case state ^. buffer . filename of
  Nothing -> return $ state & message .~ "No file name specified"
  Just filename -> do
    buffer' <- bufferToFile $ state ^. buffer
    return $ state & buffer .~ buffer' & message .~ ("Saved " ++ filename)

activeMode :: State -> Maybe Mode
activeMode state = listToMaybe $ state ^. modes

enterMode :: Mode -> State -> State
enterMode mode state = state & modes %~ (mode :)

leaveMode :: State -> State
leaveMode state = state & modes %~ tail
