module Yamte.Editor
  ( loadFile
  , reloadFile
  , saveFile
  , activeMode
  , enterMode
  , leaveMode
  ) where

import Control.Exception ( try )

import Data.Maybe ( listToMaybe )

import Lens.Micro ( (%~), (&), (.~), (?~), (^.) )

import System.IO ( readFile' )
import System.IO.Error ( isDoesNotExistError )

import Text.Parsec ( parse )

import Yamte.AST ( stringifyAST )
import Yamte.Language.Text ( parseDocument )
import Yamte.Types
  ( Mode
  , State
  , document
  , filename
  , message
  , modes
  , touched
  )

loadFile' :: String -> State -> IO State
loadFile' file state = do
  text <- readFile' file
  return $ state
         & document .~ case parse parseDocument "" text of
                         Left err -> error $ show err
                         Right ast -> ast
         & filename ?~ file
         & message .~ ("Opened " ++ file)
         & touched .~ False

handleError :: IOError -> State -> State
handleError exception state
  | isDoesNotExistError exception = state & message .~ "File does not exist."
  | otherwise = state & message .~ "Unknown error when loading file."

loadFile :: String -> State -> IO State
loadFile file state = do
  stateOrException <- try $ loadFile' file state
  case stateOrException of
    Left exception -> return $ handleError exception state
    Right state' -> return state'

reloadFile :: State -> IO State
reloadFile state = case state ^. filename of
  Nothing -> return $ state & message .~ "No file name specified"
  Just file -> loadFile file state

saveFile :: State -> IO State
saveFile state = case state ^. filename of
  Nothing -> return $ state & message .~ "No file name specified"
  Just file -> do
    writeFile file $ stringifyAST $ state ^. document
    return $ state & message .~ ("Saved " ++ file) & touched .~ False

activeMode :: State -> Maybe Mode
activeMode state = listToMaybe $ state ^. modes

enterMode :: Mode -> State -> State
enterMode mode state = state & modes %~ (mode :)

leaveMode :: State -> State
leaveMode state = state & modes %~ tail
