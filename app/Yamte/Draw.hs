module Yamte.Draw
  ( draw
  ) where

import Brick.BorderMap (Edges(..))
import Brick.Types
  ( Location(..)
  , Padding(Max)
  , Size(Greedy)
  , ViewportType(Both)
  , Widget(..)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import Brick.Util (clamp)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Table as WT
import Data.List (intercalate, intersperse)
import Data.List.Index (indexed)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Skylighting.Types (SourceLine, Syntax(sName), Token)
import Yamte.Attributes (findAttribute)
import Yamte.Editor (activeMode)
import Yamte.Types
  ( Action(..)
  , Buffer(..)
  , Mode(..)
  , Resource(..)
  , State(..)
  , Widget'
  )

modeStatus :: [Mode] -> String
modeStatus modes =
  let modeNames = reverse $ map show modes
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
    let lines :: [(Int, SourceLine)]
        lines = indexed $ bufferHighlighted $ stateBuffer state
        offset :: Int -> Int -> Int
        offset cursorPosition screenSize = cursorPosition - (screenSize `div` 2)
        (cursorLine, cursorColumn) = stateCursor state
        viewHeight = availHeight context
        viewWidth = availWidth context
        lineOffset =
          clamp 0 (length lines - viewHeight) $ offset cursorLine viewHeight
        columnOffset = max 0 $ offset cursorColumn viewWidth
        visibleLines :: [(Int, SourceLine)]
        visibleLines = take viewHeight $ drop lineOffset $ lines
        drawLineNumber :: Int -> Widget'
        drawLineNumber lineNumber = W.str $ show $ lineNumber + 1
        lineNumbers :: [Widget']
        lineNumbers = map (drawLineNumber . fst) visibleLines
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
        lineWidgets = map drawLine visibleLines
    render $
      C.vCenter $
      W.padRight Max $
      W.hBox
        [ W.vBox lineNumbers
        , W.vLimit (length visibleLines) B.vBorder
        , W.vBox lineWidgets
        ]

drawHints :: State -> Widget'
drawHints state
  | stateShowHints state =
    case activeMode state of
      Nothing -> W.emptyWidget
      (Just (FunctionMode _ _)) -> W.emptyWidget
      (Just (ActionMode _ actions)) -> drawTable actions
  | otherwise = W.emptyWidget
  where
    drawHint :: Action -> [Widget']
    drawHint (Action trigger name _) = [W.str $ show trigger, W.str name]
    drawHint (IOAction trigger name _) = [W.str $ show trigger, W.str name]
    drawTable :: [Action] -> Widget'
    drawTable actions = WT.renderTable $ WT.table $ map drawHint actions

drawMessage :: State -> Widget'
drawMessage = C.hCenter . W.str . stateMessage

draw :: State -> [Widget']
draw state = [ui]
  where
    ui =
      W.vBox
        [ drawStatus state
        , B.hBorder
        , drawViewport state <+> drawHints state
        , B.hBorder
        , drawMessage state
        ]
