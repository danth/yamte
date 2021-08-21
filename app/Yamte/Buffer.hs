module Yamte.Buffer ( bufferFromFile, bufferToFile, modifyBuffer ) where

import Data.Default.Class ( Default(..) )
import Data.Foldable ( toList )
import qualified Data.Sequence as S
import qualified Data.Text as T

import Lens.Micro ( (%~), (&), (.~), (?~), (^.) )

import Skylighting ( syntaxByName, syntaxesByFilename )
import Skylighting.Syntax ( defaultSyntaxMap )
import Skylighting.Tokenizer ( TokenizerConfig(..), tokenize )
import Skylighting.Types ( SourceLine, Syntax )

import System.IO ( readFile' )

import Yamte.Types
  ( Buffer
  , BufferText
  , filename
  , highlighted
  , syntax
  , text
  , touched
  )

setSyntax :: Buffer -> Buffer
setSyntax buffer = case buffer ^. filename of
  Nothing -> buffer & syntax .~ def
  Just filename
    -> let fileSyntax = head $ syntaxesByFilename defaultSyntaxMap filename
       in buffer & syntax .~ fileSyntax

tokenizerConfig :: TokenizerConfig
tokenizerConfig = TokenizerConfig { syntaxMap = defaultSyntaxMap
                                  , traceOutput = False
                                  }

tokenize' :: Syntax -> T.Text -> [ SourceLine ]
tokenize' syntax text
  = let throwError :: Either String [ SourceLine ] -> [ SourceLine ]
        throwError (Left e) = error e
        throwError (Right lines) = lines
    in throwError $ tokenize tokenizerConfig syntax text

highlight :: Buffer -> Buffer
highlight buffer = buffer
  & highlighted .~ tokenize' (buffer ^. syntax) (bufferToText buffer)

modifyBuffer :: (BufferText -> BufferText) -> Buffer -> Buffer
modifyBuffer f = highlight . (touched .~ True) . (text %~ f)

bufferFromString :: String -> Buffer
bufferFromString string
  = let bufferText = S.fromList $ T.lines $ T.pack string
        bufferText' = if null bufferText
                      then S.singleton T.empty
                      else bufferText
    in def & text .~ bufferText'

bufferFromFile :: String -> IO Buffer
bufferFromFile file = do
  content <- readFile' file
  return $ bufferFromString content & filename ?~ file & setSyntax & highlight

bufferToText :: Buffer -> T.Text
bufferToText buffer = T.unlines $ toList $ buffer ^. text

bufferToString :: Buffer -> String
bufferToString = T.unpack . bufferToText

bufferToFile :: Buffer -> IO Buffer
bufferToFile buffer = case buffer ^. filename of
  Nothing -> return buffer
  Just filename -> do writeFile filename $ bufferToString buffer
                      return $ buffer & touched .~ False
