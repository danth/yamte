{-# LANGUAGE OverloadedStrings #-}

module Yamte.Attributes ( attributes ) where

import Brick.AttrMap ( AttrMap, AttrName, attrMap )
import Brick.Util ( fg )

import Graphics.Vty.Attributes ( bold, defAttr, withStyle )
import Graphics.Vty.Attributes.Color

textAttribute :: AttrName
textAttribute = "text"

keywordAttribute :: AttrName
keywordAttribute = textAttribute <> "keyword"

builtinAttribute :: AttrName
builtinAttribute = textAttribute <> "builtin"

stringAttribute :: AttrName
stringAttribute = textAttribute <> "string"

numericAttribute :: AttrName
numericAttribute = textAttribute <> "numeric"

identifierAttribute :: AttrName
identifierAttribute = textAttribute <> "identifier"

attributeAttribute :: AttrName
attributeAttribute = textAttribute <> "attribute"

extensionAttribute :: AttrName
extensionAttribute = textAttribute <> "extension"

commentAttribute :: AttrName
commentAttribute = textAttribute <> "comment"

informationAttribute :: AttrName
informationAttribute = textAttribute <> "information"

warningAttribute :: AttrName
warningAttribute = textAttribute <> "warning"

errorAttribute :: AttrName
errorAttribute = textAttribute <> "error"

attributes :: AttrMap
attributes = attrMap defAttr
  [ ( textAttribute, fg white )
  , ( keywordAttribute, fg blue )
  , ( builtinAttribute, fg cyan )
  , ( stringAttribute, fg green )
  , ( numericAttribute, fg red )
  , ( identifierAttribute, fg yellow )
  , ( attributeAttribute, fg yellow )
  , ( extensionAttribute, fg magenta )
  , ( commentAttribute, defAttr `withStyle` bold )
  , ( informationAttribute, fg cyan )
  , ( warningAttribute, fg yellow )
  , ( errorAttribute, fg red )
  ]
