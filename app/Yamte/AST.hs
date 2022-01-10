module Yamte.AST ( renderAST, renderStateAST, stringifyAST ) where

import qualified Brick.Widgets.Core as W

import Data.Tree ( foldTree )
import Data.Tree.Cursor ( CursorRelativity(..), foldCursor )

import Lens.Micro ( (^.) )

import Yamte.Attributes ( cursorAttribute )
import Yamte.Types
  ( State
  , currentInput
  , document
  , SyntaxConstruct
  , AST
  , ASTCursor
  , render
  , stringify
  , Widget'
  )

renderAST :: ASTCursor -> Widget'
renderAST = foldCursor renderNode
  where
    renderNode :: CursorRelativity -> SyntaxConstruct -> [ Widget' ] -> Widget'
    renderNode IsTarget construct
      = W.visible . W.forceAttr cursorAttribute . (construct ^. render)
    renderNode _ construct = construct ^. render

renderStateAST :: State -> Widget'
renderStateAST state = foldCursor renderNode $ state ^. document
  where
    renderNode :: CursorRelativity -> SyntaxConstruct -> [ Widget' ] -> Widget'
    renderNode IsTarget construct
      = W.visible . W.forceAttr cursorAttribute
      . case state ^. currentInput of
          Nothing -> construct ^. render
          Just "" -> const $ W.str " "
          Just input -> const $ W.str input
    renderNode _ construct = construct ^. render

stringifyAST :: AST -> String
stringifyAST = foldTree (^. stringify)
