{-# LANGUAGE OverloadedStrings #-}

module Yamte.Attributes
  ( attributes
  , frameAttribute
  , findAttribute
  ) where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Util (fg, on)
import qualified Data.Map as M
import Graphics.Vty.Attributes (bold, defAttr, withStyle)
import Graphics.Vty.Attributes.Color
import Skylighting.Types (TokenType(..))

frameAttribute :: AttrName
frameAttribute = "frame"

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
attributes =
  attrMap
    defAttr
    [ (frameAttribute, black `on` white)
    , (textAttribute, fg white)
    , (keywordAttribute, fg blue)
    , (builtinAttribute, fg cyan)
    , (stringAttribute, fg green)
    , (numericAttribute, fg red)
    , (identifierAttribute, fg yellow)
    , (attributeAttribute, fg yellow)
    , (extensionAttribute, fg magenta)
    , (commentAttribute, defAttr `withStyle` bold)
    , (informationAttribute, fg cyan)
    , (warningAttribute, fg yellow)
    , (errorAttribute, fg red)
    ]

findAttribute :: TokenType -> AttrName
findAttribute tokenType = M.findWithDefault textAttribute tokenType tokenTypes
  where
    tokenTypes :: M.Map TokenType AttrName
    tokenTypes =
      M.fromList
        [ (ControlFlowTok, keywordAttribute)
        , (KeywordTok, keywordAttribute)
        , (BuiltInTok, builtinAttribute)
        , (OperatorTok, builtinAttribute)
        , (BaseNTok, numericAttribute)
        , (DecValTok, numericAttribute)
        , (FloatTok, numericAttribute)
        , (SpecialCharTok, numericAttribute)
        , (CharTok, stringAttribute)
        , (DataTypeTok, stringAttribute)
        , (StringTok, stringAttribute)
        , (SpecialStringTok, stringAttribute)
        , (VerbatimStringTok, stringAttribute)
        , (ConstantTok, identifierAttribute)
        , (FunctionTok, identifierAttribute)
        , (VariableTok, identifierAttribute)
        , (AttributeTok, attributeAttribute)
        , (ExtensionTok, extensionAttribute)
        , (ImportTok, extensionAttribute)
        , (PreprocessorTok, extensionAttribute)
        , (AnnotationTok, commentAttribute)
        , (CommentTok, commentAttribute)
        , (CommentVarTok, commentAttribute)
        , (DocumentationTok, commentAttribute)
        , (RegionMarkerTok, commentAttribute)
        , (InformationTok, informationAttribute)
        , (AlertTok, warningAttribute)
        , (WarningTok, warningAttribute)
        , (ErrorTok, errorAttribute)
        ]
