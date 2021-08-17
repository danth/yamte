module Yamte.Draw (draw) where

import qualified Data.Text as T
import Data.List (intercalate)
import Data.List.Index (imap)
import Brick.Types (Widget, ViewportType(Both), Location(..), Padding(Max))
import qualified Brick.Widgets.Core as W
import Brick.Widgets.Core ((<+>))
import Yamte.Types (Mode(..), State(..), Buffer(..), Widget', Resource(..))
import Yamte.Attributes (frameAttribute, findAttribute)
import Skylighting.Types (SourceLine, Token, Syntax(sName))

modeStatus :: [Mode] -> String
modeStatus modes =
  let modeNames = reverse $ map (\(Mode name _) -> name) modes
   in (intercalate " → " modeNames) ++ " mode"

drawStatus :: State -> Widget'
drawStatus state = W.withAttr frameAttribute
                 $ W.padRight Max
                 $ W.str
                 $ intercalate " • "
                 $ elements
    where buffer = stateBuffer state
          elements = [ (case bufferFilename buffer of
                          Nothing -> "[No name]"
                          Just filename -> filename)
                     , (if bufferTouched buffer
                          then "Touched"
                          else "Untouched")
                     , (show (length $ bufferText buffer) ++ " lines")
                     , (modeStatus $ stateModes state)
                     , ((T.unpack $ sName $ bufferSyntax buffer) ++ " highlighting")
                     ]


drawBuffer :: State -> Widget'
drawBuffer state = W.vBox $ imap drawLine lines
  where
    (cursorLine, cursorColumn) = stateCursor state

    drawLine :: Int -> SourceLine -> Widget'
    drawLine lineNumber tokens = sidebar <+> line
      where
        sidebar = W.withAttr frameAttribute
                $ W.hLimit 4
                $ W.padRight Max
                $ W.str (show $ lineNumber + 1)

        setCursor :: Widget' -> Widget'
        setCursor = if lineNumber == cursorLine
                       then W.showCursor FileCursor (Location (cursorColumn, 0))
                       else id

        makeVisible :: Widget' -> Widget'
        makeVisible = if lineNumber == cursorLine
                         then W.visibleRegion (Location (cursorColumn, 0)) (1, 1)
                         else id

        drawToken :: Token -> Widget'
        drawToken (tokenType, tokenText) = W.withAttr (findAttribute tokenType)
                                         $ W.txt tokenText

        line = setCursor $ makeVisible $ W.hBox $ map drawToken tokens

    lines :: [SourceLine]
    lines = bufferHighlighted $ stateBuffer state

drawViewport :: State -> Widget'
drawViewport state = W.viewport FileViewport Both $ drawBuffer state

drawMessage :: State -> Widget'
drawMessage = W.withAttr frameAttribute . W.padRight Max . W.str . stateMessage

draw :: State -> [Widget']
draw state = [ui]
  where ui = W.vBox [ drawStatus state
                    , drawViewport state
                    , drawMessage state
                    ]
