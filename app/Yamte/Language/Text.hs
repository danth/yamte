{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yamte.Language.Text ( parseWord, parseLine, parseDocument ) where

import qualified Brick.Widgets.Core as W

import Data.Char ( isSpace )
import Data.Tree ( Tree(..) )

import Text.Parsec ( ParsecT, Stream, (<|>), many, many1, manyTill )
import Text.Parsec.Char ( newline, satisfy, space )

import Yamte.Types ( AST, SyntaxConstruct(..) )

notSpace :: (Stream s m Char) => ParsecT s u m Char
notSpace = satisfy (not . isSpace)

parseWord :: (Stream s m Char) => ParsecT s u m AST
parseWord = do text <- many1 notSpace <|> many1 space
               return
                 $ Node (SyntaxConstruct { _render = const $ W.str text
                                         , _stringify = const text
                                         }) []

parseLine :: (Stream s m Char) => ParsecT s u m AST
parseLine = Node
  (SyntaxConstruct
   { _render = \words -> case words of [] -> W.vLimit 1 $ W.fill ' '
                                       _ -> W.hBox words
   , _stringify = concat
   })
  <$> manyTill parseWord newline

parseDocument :: (Stream s m Char) => ParsecT s u m AST
parseDocument = Node (SyntaxConstruct { _render = W.vBox
                                      , _stringify = unlines
                                      })
  <$> many parseLine