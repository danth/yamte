module Yamte.Draw ( draw ) where

import Brick.Types
  ( Context
  , Location(..)
  , Padding(..)
  , Result(image)
  , Size(..)
  , Widget(..)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import Brick.Util ( clamp )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Brick.Widgets.Core ( (<+>) )
import qualified Brick.Widgets.Table as WT

import Data.Function ( on )
import Data.List ( groupBy, intercalate, intersperse )
import Data.List.Index ( indexed )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T

import Graphics.Vty.Image ( imageWidth )

import Lens.Micro ( (^.), (^..), (^?!), _head, each )

import Skylighting.Types ( SourceLine, Syntax(sName), Token )

import Yamte.Attributes ( findAttribute )
import Yamte.Buffer ( text )
import Yamte.Editor ( activeMode )
import Yamte.Types
  ( Action(..)
  , description
  , trigger
  , Mode(..)
  , ModifiedKey
  , Resource(..)
  , State
  , Widget'
  , buffer
  , cursor
  , filename
  , highlighted
  , message
  , modes
  , showHints
  , syntax
  , touched
  )

padToWidth :: Int -> Widget' -> Widget'
padToWidth desiredWidth widget = Widget Fixed (vSize widget)
  $ do result <- render widget
       let resultWidget :: Widget'
           resultWidget = Widget Fixed Fixed $ return result
           actualWidth :: Int
           actualWidth = imageWidth $ image result
           finalWidth :: Int
           finalWidth = max desiredWidth actualWidth
           padding :: Int
           padding = finalWidth - actualWidth
       render $ W.padRight (Pad padding) resultWidget

modeStatus :: [ Mode ] -> String
modeStatus modes = let modeNames = reverse $ map show modes
                   in intercalate " → " modeNames ++ " mode"

drawStatus :: State -> Widget'
drawStatus state = C.hCenter $ W.hBox $ intersperse separator widgets
  where
    elements :: [ String ]
    elements = [ fromMaybe "[No name]" $ state ^. buffer . filename
               , if state ^. buffer . touched
                 then "Touched"
                 else "Untouched"
               , show (length $ state ^. buffer . text) ++ " lines"
               , modeStatus $ state ^. modes
               , T.unpack (sName $ state ^. buffer . syntax) ++ " highlighting"
               ]

    widgets :: [ Widget' ]
    widgets = map W.str elements

    separator :: Widget'
    separator = W.str " • "

drawViewport' :: State -> Context -> Widget'
drawViewport' state context = C.vCenter
  $ W.padRight Max
  $ W.hBox [ W.vBox lineNumbers
           , W.vLimit (length visibleLines) B.vBorder
           , W.vBox lineWidgets
           ]
  where
    cursorLine :: Int
    cursorColumn :: Int
    ( cursorLine, cursorColumn ) = state ^. cursor

    viewHeight :: Int
    viewHeight = availHeight context

    viewWidth :: Int
    viewWidth = availWidth context

    lines :: [ ( Int, SourceLine ) ]
    lines = indexed $ state ^. buffer . highlighted

    offset :: Int -> Int -> Int
    offset cursorPosition screenSize = cursorPosition - (screenSize `div` 2)

    lineOffset :: Int
    lineOffset = clamp 0 (length lines - viewHeight)
      $ offset cursorLine viewHeight

    columnOffset :: Int
    columnOffset = max 0 $ offset cursorColumn viewWidth

    marginWidth :: Int
    marginWidth = length (show $ length lines) + 1

    drawLineNumber :: Int -> Widget'
    drawLineNumber = padToWidth marginWidth . W.str . show . (+ 1)

    drawToken :: Token -> Widget'
    drawToken ( tokenType, tokenText ) = W.withAttr (findAttribute tokenType)
      $ W.txt tokenText

    drawLine :: ( Int, SourceLine ) -> Widget'
    drawLine ( lineNumber, tokens ) = W.cropLeftBy columnOffset
      $ setCursor
      $ case tokens of [] -> W.vLimit 1 $ W.fill ' '
                       t -> W.hBox $ map drawToken t
      where setCursor :: Widget' -> Widget'
            setCursor
              = if lineNumber == cursorLine
                then W.showCursor FileCursor (Location ( cursorColumn, 0 ))
                else id

    visibleLines :: [ ( Int, SourceLine ) ]
    visibleLines = take viewHeight $ drop lineOffset lines

    lineNumbers :: [ Widget' ]
    lineNumbers = map (drawLineNumber . fst) visibleLines

    lineWidgets :: [ Widget' ]
    lineWidgets = map drawLine visibleLines

drawViewport :: State -> Widget'
drawViewport state
  = Widget Greedy Greedy $ render . drawViewport' state =<< getContext

type Hint = ( [ ModifiedKey ], String )

drawHints :: State -> Widget'
drawHints state
  | state ^. showHints = case activeMode state of
    Nothing -> W.emptyWidget
    (Just (FunctionMode _ _)) -> W.emptyWidget
    (Just (ActionMode _ actions)) -> drawHints actions
  | otherwise = W.emptyWidget
  where buildHint :: [ Action ] -> Hint
        buildHint actions
          = ( actions ^.. each . trigger, actions ^?! _head . description )

        buildHints :: [ Action ] -> [ Hint ]
        buildHints = map buildHint . groupBy ((==) `on` (^. description))

        drawHint :: Hint -> [ Widget' ]
        drawHint ( keys, description ) = map (W.padLeftRight 1 . W.str)
          [ unwords $ map show keys, description ]

        drawTable :: [ [ Widget' ] ] -> Widget'
        drawTable = WT.renderTable . WT.rowBorders False . WT.table

        drawHints :: [ Action ] -> Widget'
        drawHints = drawTable . map drawHint . buildHints

drawMessage :: State -> Widget'
drawMessage state = C.hCenter $ W.str $ state ^. message

draw :: State -> [ Widget' ]
draw state = [ ui ]
  where ui = W.vBox [ drawStatus state
                    , B.hBorder
                    , drawViewport state <+> drawHints state
                    , B.hBorder
                    , drawMessage state
                    ]
