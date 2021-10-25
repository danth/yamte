{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yamte.Language.Text ( parseWord, parseLine, parseDocument ) where

import qualified Brick.Widgets.Core as W

import Data.Char ( isSpace )
import Data.Tree ( Tree(..) )

import Text.Parsec ( ParsecT, Stream, (<|>), many, many1, endBy )
import Text.Parsec.Char ( newline, satisfy )

import Yamte.Types ( AST, SyntaxConstruct(..) )

space :: (Stream s m Char) => ParsecT s u m Char
space = satisfy (\c -> isSpace c && (c /= '\r') && (c /= '\n'))

notSpace :: (Stream s m Char) => ParsecT s u m Char
notSpace = satisfy (not . isSpace)

parseWord :: (Stream s m Char) => ParsecT s u m AST
parseWord = do text <- many1 notSpace <|> many1 space
               return
                 $ Node (SyntaxConstruct { _render = const $ W.str text
                                         , _stringify = const text
                                         , _parser = parseWord
                                         }) []

parseLine :: (Stream s m Char) => ParsecT s u m AST
parseLine = Node
  (SyntaxConstruct { _render = \words -> case words of [] -> W.str " "
                                                       _ -> W.hBox words
                   , _stringify = concat
                   , _parser = parseLine
                   })
  <$> many parseWord

parseDocument :: (Stream s m Char) => ParsecT s u m AST
parseDocument = Node (SyntaxConstruct { _render = W.vBox
                                      , _stringify = unlines
                                      , _parser = parseDocument
                                      })
  <$> parseLine `endBy` newline
