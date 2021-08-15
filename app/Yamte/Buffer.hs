module Yamte.Buffer
  ( Buffer(..)
  , BufferText
  , emptyBuffer
  , bufferFromFile
  , bufferToFile
  , modifyBuffer
  ) where

import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Skylighting (syntaxByName, syntaxesByFilename)
import Skylighting.Syntax (defaultSyntaxMap)
import Skylighting.Tokenizer (TokenizerConfig(..), tokenize)
import Skylighting.Types (SourceLine, Syntax)
import System.IO (readFile')

type BufferText = S.Seq T.Text

data Buffer =
  Buffer
    { bufferText :: BufferText
    , bufferHighlighted :: [SourceLine]
    , bufferSyntax :: Syntax
    , bufferTouched :: Bool
    , bufferFilename :: Maybe String
    }

emptyBuffer :: Buffer
emptyBuffer =
  Buffer
    { bufferText = S.singleton T.empty
    , bufferHighlighted = [[]]
    , bufferSyntax = defaultSyntax
    , bufferTouched = False
    , bufferFilename = Nothing
    }

defaultSyntax :: Syntax
defaultSyntax =
  case syntaxByName defaultSyntaxMap (T.pack "Default") of
    Nothing -> error "Default syntax does not exist"
    Just syntax -> syntax

setSyntax :: Buffer -> Buffer
setSyntax buffer =
  case bufferFilename buffer of
    Nothing -> buffer {bufferSyntax = defaultSyntax}
    Just filename ->
      let syntax = head $ syntaxesByFilename defaultSyntaxMap filename
       in buffer {bufferSyntax = syntax}

tokenizerConfig :: TokenizerConfig
tokenizerConfig =
  TokenizerConfig {syntaxMap = defaultSyntaxMap, traceOutput = False}

tokenize' :: Syntax -> T.Text -> [SourceLine]
tokenize' syntax text =
  let throwError :: Either String [SourceLine] -> [SourceLine]
      throwError (Left e) = error e
      throwError (Right lines) = lines
   in throwError $ tokenize tokenizerConfig syntax text

highlight :: Buffer -> Buffer
highlight buffer =
  let syntax = bufferSyntax buffer
      text = bufferToText buffer
   in buffer {bufferHighlighted = tokenize' syntax text}

modifyBuffer :: (BufferText -> BufferText) -> Buffer -> Buffer
modifyBuffer f buffer =
  highlight $ buffer {bufferText = f $ bufferText buffer, bufferTouched = True}

bufferFromString :: String -> Buffer
bufferFromString string =
  let text = S.fromList $ T.lines $ T.pack string
      text' =
        if length text == 0
          then S.singleton T.empty
          else text
   in emptyBuffer {bufferText = text'}

bufferFromFile :: String -> IO Buffer
bufferFromFile filename = do
  content <- readFile' filename
  let buffer = bufferFromString content
      buffer' = buffer {bufferFilename = Just filename}
  return $ highlight $ setSyntax buffer'

bufferToText :: Buffer -> T.Text
bufferToText = T.unlines . toList . bufferText

bufferToString :: Buffer -> String
bufferToString = T.unpack . bufferToText

bufferToFile :: Buffer -> IO Buffer
bufferToFile buffer =
  case bufferFilename buffer of
    Nothing -> return buffer
    Just filename -> do
      writeFile filename $ bufferToString buffer
      return $ buffer {bufferTouched = False}
