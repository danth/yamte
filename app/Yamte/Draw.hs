module Yamte.Draw ( draw ) where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Brick.Widgets.Core ( (<+>) )
import qualified Brick.Widgets.Table as WT

import Data.Function ( on )
import Data.List ( groupBy, intercalate, intersperse )
import Data.Maybe ( fromMaybe )

import Lens.Micro ( (^.), (^..), (^?!), _head, each )

import Yamte.AST ( renderAST )
import Yamte.Editor ( activeMode )
import Yamte.Types
  ( Action(..)
  , description
  , trigger
  , Mode(..)
  , ModifiedKey
  , State
  , Widget'
  , document
  , filename
  , message
  , modes
  , showHints
  , touched
  )

modeStatus :: [ Mode ] -> String
modeStatus modes = let modeNames = reverse $ map show modes
                   in intercalate " → " modeNames ++ " mode"

drawStatus :: State -> Widget'
drawStatus state = C.hCenter $ W.hBox $ intersperse separator widgets
  where elements :: [ String ]
        elements = [ fromMaybe "[No name]" $ state ^. filename
                   , if state ^. touched
                     then "Touched"
                     else "Untouched"
                   , modeStatus $ state ^. modes
                   ]

        widgets :: [ Widget' ]
        widgets = map W.str elements

        separator :: Widget'
        separator = W.str " • "

drawViewport :: State -> Widget'
drawViewport state = renderAST $ state ^. document

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
