module Yamte.Display
  ( DisplayState
  , createDisplayState
  , draw
  , Yamte.Display.getEvent
  ) where

import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.Index (imap)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import Skylighting (Syntax(..))
import Skylighting.Syntax (defaultSyntaxMap)
import Skylighting.Tokenizer (TokenizerConfig(..), tokenize)
import Skylighting.Types (SourceLine, Token, TokenType(..), defaultFormatOpts)
import UI.NCurses
import Yamte.Editor (Buffer, Cursor, Mode(Mode), State(..), activeMode)

type Style = M.Map TokenType ColorID

type View = (Int, Int)

data DisplayState =
  DisplayState
    { statusWindow :: Window
    , sidebarWindow :: Window
    , bufferWindow :: Window
    , messageWindow :: Window
    , style :: Style
    , view :: View
    }

invertBackground :: Update ()
invertBackground = setBackground $ Glyph ' ' [AttributeReverse]

createStyle :: Curses Style
createStyle = do
  red <- newColorID ColorRed ColorBlack 1
  green <- newColorID ColorGreen ColorBlack 2
  yellow <- newColorID ColorYellow ColorBlack 3
  blue <- newColorID ColorBlue ColorBlack 4
  magenta <- newColorID ColorMagenta ColorBlack 5
  cyan <- newColorID ColorCyan ColorBlack 6
  comment <- newColorID ColorBlack ColorBlue 7
  return $
    M.fromList
      [ (KeywordTok, blue)
      , (DataTypeTok, green)
      , (DecValTok, red)
      , (BaseNTok, red)
      , (FloatTok, red)
      , (ConstantTok, magenta)
      , (CharTok, green)
      , (SpecialCharTok, red)
      , (StringTok, green)
      , (VerbatimStringTok, green)
      , (SpecialStringTok, green)
      , (ImportTok, magenta)
      , (CommentTok, comment)
      , (DocumentationTok, comment)
      , (AnnotationTok, comment)
      , (CommentVarTok, comment)
      , (OtherTok, defaultColorID)
      , (FunctionTok, yellow)
      , (VariableTok, yellow)
      , (ControlFlowTok, cyan)
      , (OperatorTok, cyan)
      , (BuiltInTok, cyan)
      , (ExtensionTok, magenta)
      , (PreprocessorTok, magenta)
      , (AttributeTok, yellow)
      , (RegionMarkerTok, comment)
      , (InformationTok, cyan)
      , (WarningTok, yellow)
      , (AlertTok, yellow)
      , (ErrorTok, red)
      , (NormalTok, defaultColorID)
      ]

createDisplayState :: Curses DisplayState
createDisplayState = do
  (rows, columns) <- screenSize
  status <- newWindow 1 columns 0 0
  updateWindow status invertBackground
  sidebar <- newWindow (rows - 2) 4 1 0
  updateWindow sidebar invertBackground
  buffer <- newWindow (rows - 2) (columns - 4) 1 4
  message <- newWindow 1 columns (rows - 1) 0
  updateWindow message invertBackground
  style <- createStyle
  return
    DisplayState
      { statusWindow = status
      , sidebarWindow = sidebar
      , bufferWindow = buffer
      , messageWindow = message
      , style = style
      , view = (0, 0)
      }

scrollAxis :: Int -> Int -> Int -> Int
scrollAxis cursorPosition scrollOffset displaySize
  | cursorPosition < scrollOffset = cursorPosition
  | cursorPosition >= scrollOffset + displaySize =
    cursorPosition - displaySize + 1
  | otherwise = scrollOffset

scroll :: DisplayState -> State -> Curses DisplayState
scroll displayState state =
  updateWindow (bufferWindow displayState) $ do
    let (row, column) = stateCursor state
        (rowOffset, columnOffset) = view displayState
    (rows, columns) <- windowSize
    let view' =
          ( scrollAxis row rowOffset (fromIntegral rows)
          , scrollAxis column columnOffset (fromIntegral columns))
    return displayState {view = view'}

statusLine :: [String] -> String
statusLine = intercalate " • "

modeStatus :: [Mode] -> String
modeStatus modes =
  let modeNames = reverse $ map (\(Mode name _) -> name) modes
   in (intercalate " → " modeNames) ++ " mode"

tokenizerConfig :: TokenizerConfig
tokenizerConfig =
  TokenizerConfig {syntaxMap = defaultSyntaxMap, traceOutput = False}

highlightBuffer :: Buffer -> Maybe Syntax -> [Either T.Text SourceLine]
highlightBuffer buffer Nothing = map Left $ toList buffer
highlightBuffer buffer (Just syntax) =
  case tokenize tokenizerConfig syntax (T.unlines $ toList buffer) of
    (Left error) -> map Left $ toList buffer
    (Right sourceLines) -> map Right sourceLines

drawStatus :: DisplayState -> State -> Curses ()
drawStatus displayState state =
  updateWindow (statusWindow displayState) $ do
    clear
    moveCursor 0 4
    (rows, columns) <- windowSize
    drawString $
      take ((fromIntegral columns) - 5) $
      statusLine
        [ (case stateFilename state of
             Nothing -> "[No name]"
             Just filename -> filename)
        , (if stateTouched state
             then "Touched"
             else "Untouched")
        , (show (length $ stateBuffer state) ++ " lines")
        , (modeStatus $ stateModes state)
        , (case stateSyntax state of
             Nothing -> "No highlighting"
             Just syntax -> (T.unpack $ sName syntax) ++ " highlighting")
        ]

drawSidebar ::
     DisplayState -> [(Int, Int, Either T.Text SourceLine)] -> Curses ()
drawSidebar displayState lines =
  updateWindow (sidebarWindow displayState) $ do
    clear
    forM_ lines $ \(screenIndex, lineNumber, line) -> do
      moveCursor (toInteger screenIndex) 0
      drawString $ show lineNumber

drawToken :: DisplayState -> Token -> Update ()
drawToken displayState (tokenType, tokenText) =
  let color = M.findWithDefault defaultColorID tokenType $ style displayState
   in do setColor color
         drawText tokenText

drawLine :: DisplayState -> (Int, Int, Either T.Text SourceLine) -> Update ()
drawLine displayState (screenIndex, lineNumber, line) = do
  moveCursor (toInteger screenIndex) 0
  (rows, columns) <- windowSize
  case line of
    Left text ->
      let columnOffset = snd $ view displayState
          trimmedLine = T.take (fromIntegral columns) $ T.drop columnOffset text
       in drawText trimmedLine
    Right tokens -> forM_ tokens $ drawToken displayState

drawBuffer ::
     DisplayState -> [(Int, Int, Either T.Text SourceLine)] -> Curses ()
drawBuffer displayState lines =
  let updateBuffer :: Update () -> Curses ()
      updateBuffer = updateWindow $ bufferWindow displayState
      drawLine' ::
           (Int, Int, Either T.Text SourceLine)
        -> Curses (Either CursesException ())
      drawLine' = tryCurses . updateBuffer . drawLine displayState
   in do updateBuffer clear
         forM_ lines drawLine'

drawMessage :: DisplayState -> State -> Curses ()
drawMessage displayState state =
  updateWindow (messageWindow displayState) $ do
    clear
    moveCursor 0 4
    (rows, columns) <- windowSize
    drawString $ take ((fromIntegral columns) - 5) $ stateMessage state

positionCursor :: DisplayState -> Cursor -> Curses ()
positionCursor displayState (row, column) =
  let (rowOffset, columnOffset) = view displayState
      row' = (fromIntegral row) - rowOffset
      column' = (fromIntegral column) - columnOffset
   in updateWindow (bufferWindow displayState) $
      moveCursor (fromIntegral row') (fromIntegral column')

draw' :: DisplayState -> State -> Curses ()
draw' displayState state = do
  (rows, columns) <- updateWindow (bufferWindow displayState) windowSize
  let (rowOffset, columnOffset) = view displayState
      highlightedBuffer :: [Either T.Text SourceLine]
      highlightedBuffer =
        highlightBuffer (stateBuffer state) (stateSyntax state)
      scrolledBuffer :: [Either T.Text SourceLine]
      scrolledBuffer =
        take (fromIntegral rows) $ drop rowOffset highlightedBuffer
      lines :: [(Int, Int, Either T.Text SourceLine)]
      lines = imap (\i l -> (i, i + rowOffset + 1, l)) scrolledBuffer
  drawStatus displayState state
  drawSidebar displayState lines
  drawBuffer displayState lines
  drawMessage displayState state
  positionCursor displayState (stateCursor state)
  render

draw :: DisplayState -> State -> Curses DisplayState
draw displayState state = do
  displayState' <- scroll displayState state
  draw' displayState' state
  return displayState'

getEvent :: DisplayState -> Curses (Maybe Event)
getEvent displayState = UI.NCurses.getEvent (bufferWindow displayState) Nothing
