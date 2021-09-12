module Yamte.AST ( renderAST, stringifyAST ) where

import Brick.Widgets.Core ( forceAttr, visible )

import Data.Tree ( foldTree )
import Data.Tree.Cursor ( CursorRelativity(..), foldCursor )

import Lens.Micro ( (^.) )

import Yamte.Attributes ( cursorAttribute )
import Yamte.Types
  ( SyntaxConstruct
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
      = visible . forceAttr cursorAttribute . (construct ^. render)
    renderNode _ construct = construct ^. render

stringifyAST :: AST -> String
stringifyAST = foldTree (^. stringify)
