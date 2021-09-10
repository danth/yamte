module Yamte.AST ( renderAST, stringifyAST ) where

import Data.Tree ( foldTree )

import Lens.Micro ( (^.) )

import Yamte.Types ( AST, render, stringify, Widget' )

renderAST :: AST -> Widget'
renderAST = foldTree $ \syntaxConstruct -> syntaxConstruct ^. render

stringifyAST :: AST -> String
stringifyAST = foldTree $ \syntaxConstruct -> syntaxConstruct ^. stringify
