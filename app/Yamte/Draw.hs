module Yamte.Draw ( draw ) where

import Brick.Types ( ViewportType(Both) )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import Brick.Widgets.Core ( (<+>), (<=>) )

import Data.Function ( on )
import Data.List ( groupBy )
import Data.Maybe ( fromMaybe )
import Data.Stack ( stackPeek )

import Lens.Micro ( (^.), (^..), (^?!), _head, each )

import Yamte.AST ( renderStateAST )
import Yamte.Mode ( activeMode )
import Yamte.Types
  ( Action(..)
  , description
  , trigger
  , Mode(..)
  , ModifiedKey
  , Resource(..)
  , State
  , Widget'
  , filename
  , message
  , modeStack
  , touched
  )

drawViewport :: State -> Widget'
drawViewport state = W.viewport MainViewport Both $ renderStateAST state

drawFilename :: State -> Widget'
drawFilename state = W.padLeftRight 1 $ W.str $ file ++ flag
  where file :: String
        file = fromMaybe "[No name]" $ state ^. filename

        flag :: String
        flag = if state ^. touched
               then " *"
               else ""

drawMessage :: State -> Widget'
drawMessage state = W.padAll 1 $ W.strWrap $ state ^. message

type Hint = ( [ ModifiedKey ], String )

getActions :: State -> [ Action ]
getActions state = case activeMode state of
  Nothing -> []
  (Just (FunctionMode _ _)) -> []
  (Just (ActionMode _ actions)) -> actions

buildHints :: [ Action ] -> [ Hint ]
buildHints = map buildHint . groupBy ((==) `on` (^. description))
  where buildHint :: [ Action ] -> Hint
        buildHint actions
          = ( actions ^.. each . trigger, actions ^?! _head . description )

drawHints :: [ Hint ] -> Widget'
drawHints [] = C.hCenter $ W.str "No hints available"
drawHints hints = W.hBox
  [ W.vBox $ map (W.str . unwords . map show . fst) hints
  , W.vLimit (length hints) $ W.padLeftRight 1 B.vBorder
  , W.vBox $ map (W.str . snd) hints
  ]

drawHints' :: State -> Widget'
drawHints' = drawHints . buildHints . getActions

modeName :: State -> String
modeName state = case stackPeek $ state ^. modeStack of
  Nothing -> "No mode"
  Just mode -> show mode ++ " mode"

drawMode :: State -> Widget'
drawMode state = W.joinBorders
  $ B.border
  $ W.vBox [ C.hCenter $ W.str $ modeName state
           , B.hBorder
           , W.padLeftRight 1 $ drawHints' state
           ]

draw :: State -> [ Widget' ]
draw state = [ ui ]
  where
    ui :: Widget'
    ui = page <+> sidebar

    page :: Widget'
    page = B.borderWithLabel (drawFilename state) (drawViewport state)

    sidebar :: Widget'
    sidebar
      = W.padLeftRight 1 $ W.hLimit 45 $ drawMode state <=> drawMessage state
