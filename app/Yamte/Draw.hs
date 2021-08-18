module Yamte.Draw
  ( draw
  ) where

import Brick.BorderMap (Edges(..))
import Brick.Types (Location(..), Padding(Max), ViewportType(Both), Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Data.List (intercalate, intersperse)
import Data.List.Index (imap)
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

drawBuffer :: State -> Widget'
drawBuffer state = W.vBox $ imap drawLine lines
  where
    (cursorLine, cursorColumn) = stateCursor state
    singleVerticalBorder :: Widget'
    singleVerticalBorder = B.joinableBorder $ Edges True True False False
    drawLine :: Int -> SourceLine -> Widget'
    drawLine lineNumber tokens = W.hBox [sidebar, singleVerticalBorder, line]
      where
        sidebar :: Widget'
        sidebar = W.hLimit 4 $ W.padRight Max $ W.str (show $ lineNumber + 1)
        setCursor :: Widget' -> Widget'
        setCursor =
          if lineNumber == cursorLine
            then W.showCursor FileCursor (Location (cursorColumn, 0))
            else id
        makeVisible :: Widget' -> Widget'
        makeVisible =
          if lineNumber == cursorLine
            then W.visibleRegion (Location (cursorColumn, 0)) (1, 1)
            else id
        drawToken :: Token -> Widget'
        drawToken (tokenType, tokenText) =
          W.withAttr (findAttribute tokenType) $ W.txt tokenText
        line :: Widget'
        line = setCursor $ makeVisible $ W.hBox $ map drawToken tokens
    lines :: [SourceLine]
    lines = bufferHighlighted $ stateBuffer state

drawViewport :: State -> Widget'
drawViewport state = W.viewport FileViewport Both $ drawBuffer state

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
