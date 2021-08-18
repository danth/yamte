module Yamte.Draw
  ( draw
  ) where

import Brick.BorderMap (Edges(..))
import Brick.Types
  ( Location(..)
  , Padding(..)
  , Size(Greedy)
  , ViewportType(Both)
  , Widget(..)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Data.List (intercalate, intersperse)
import Data.List.Index (indexed)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Skylighting.Types (SourceLine, Syntax(sName), Token)
import Yamte.Attributes (findAttribute)
import Yamte.Types (Buffer(..), Mode(..), Resource(..), State(..), Widget')

modeStatus :: [Mode] -> String
modeStatus modes =
  let modeNames = reverse $ map (\(Mode name _) -> name) modes
   in intercalate " → " modeNames ++ " mode"

drawStatus :: State -> Widget'
drawStatus state = C.hCenter $ W.hBox $ intersperse separator widgets
  where
    buffer = stateBuffer state
    elements :: [String]
    elements =
      [ fromMaybe "[No name]" $ bufferFilename buffer
      , if bufferTouched buffer
          then "Touched"
          else "Untouched"
      , show (length $ bufferText buffer) ++ " lines"
      , modeStatus $ stateModes state
      , T.unpack (sName $ bufferSyntax buffer) ++ " highlighting"
      ]
    widgets :: [Widget']
    widgets = map W.str elements
    separator :: Widget'
    separator = W.str " • "

drawViewport :: State -> Widget'
drawViewport state =
  Widget Greedy Greedy $ do
    context <- getContext
    let offset :: Int -> Int -> Int
        offset cursorPosition screenSize =
          max 0 $ cursorPosition - (screenSize `div` 2)
        (cursorLine, cursorColumn) = stateCursor state
        viewHeight = availHeight context
        viewWidth = availWidth context
        lineOffset = offset cursorLine viewHeight
        columnOffset = offset cursorColumn viewWidth
        lines :: [(Int, SourceLine)]
        lines =
          take viewHeight $
          drop lineOffset $ indexed $ bufferHighlighted $ stateBuffer state
        drawLineNumber :: Int -> Widget'
        drawLineNumber lineNumber = W.str $ show $ lineNumber + 1
        lineNumbers :: [Widget']
        lineNumbers = map (drawLineNumber . fst) lines
        drawToken :: Token -> Widget'
        drawToken (tokenType, tokenText) =
          W.withAttr (findAttribute tokenType) $ W.txt tokenText
        drawLine :: (Int, SourceLine) -> Widget'
        drawLine (lineNumber, tokens) =
          W.cropLeftBy columnOffset $
          setCursor $
          case tokens of
            [] -> W.vLimit 1 $ W.fill ' '
            t -> W.hBox $ map drawToken t
          where
            setCursor :: Widget' -> Widget'
            setCursor =
              if lineNumber == cursorLine
                then W.showCursor FileCursor (Location (cursorColumn, 0))
                else id
        lineWidgets :: [Widget']
        lineWidgets = map drawLine lines
        padding :: Widget' -> Widget'
        padding = W.padBottom Max . W.padTop (Pad topPadding)
          where topPadding = max 0 $ (viewHeight `div` 2) - cursorLine
    render $ padding $ W.hBox [W.vBox lineNumbers, W.vLimit (length lines) B.vBorder, W.vBox lineWidgets]

drawMessage :: State -> Widget'
drawMessage = C.hCenter . W.str . stateMessage

draw :: State -> [Widget']
draw state = [ui]
  where
    ui =
      W.vBox
        [ drawStatus state
        , B.hBorder
        , drawViewport state
        , B.hBorder
        , drawMessage state
        ]
