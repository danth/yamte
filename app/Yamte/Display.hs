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
import Skylighting.Types (SourceLine, Syntax(sName), Token, TokenType(..))
import UI.NCurses
import Yamte.Buffer (Buffer(..))
import Yamte.Editor (Cursor, Mode(Mode), State(..), activeMode)

type Style = M.Map TokenType (ColorID, [Attribute])

type NumberedLine = (Int, Int, SourceLine)

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
  return $
    M.fromList
      [ (KeywordTok, (blue, []))
      , (DataTypeTok, (green, []))
      , (DecValTok, (red, []))
      , (BaseNTok, (red, []))
      , (FloatTok, (red, []))
      , (ConstantTok, (magenta, []))
      , (CharTok, (green, []))
      , (SpecialCharTok, (red, []))
      , (StringTok, (green, []))
      , (VerbatimStringTok, (green, []))
      , (SpecialStringTok, (green, []))
      , (ImportTok, (magenta, []))
      , (CommentTok, (defaultColorID, [AttributeBold]))
      , (DocumentationTok, (defaultColorID, [AttributeBold]))
      , (AnnotationTok, (defaultColorID, [AttributeBold]))
      , (CommentVarTok, (defaultColorID, [AttributeBold]))
      , (OtherTok, (defaultColorID, []))
      , (FunctionTok, (yellow, []))
      , (VariableTok, (yellow, []))
      , (ControlFlowTok, (cyan, []))
      , (OperatorTok, (cyan, []))
      , (BuiltInTok, (cyan, []))
      , (ExtensionTok, (magenta, []))
      , (PreprocessorTok, (magenta, []))
      , (AttributeTok, (yellow, []))
      , (RegionMarkerTok, (defaultColorID, [AttributeBold]))
      , (InformationTok, (cyan, []))
      , (WarningTok, (yellow, []))
      , (AlertTok, (yellow, []))
      , (ErrorTok, (red, []))
      , (NormalTok, (defaultColorID, []))
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

drawStatus :: DisplayState -> State -> Curses ()
drawStatus displayState state =
  let buffer = stateBuffer state
   in updateWindow (statusWindow displayState) $ do
        clear
        moveCursor 0 4
        (rows, columns) <- windowSize
        drawString $
          take ((fromIntegral columns) - 5) $
          statusLine
            [ (case bufferFilename buffer of
                 Nothing -> "[No name]"
                 Just filename -> filename)
            , (if bufferTouched buffer
                 then "Touched"
                 else "Untouched")
            , (show (length $ bufferText buffer) ++ " lines")
            , (modeStatus $ stateModes state)
            , ((T.unpack $ sName $ bufferSyntax buffer) ++ " highlighting")
            ]

drawSidebar :: DisplayState -> [NumberedLine] -> Curses ()
drawSidebar displayState lines =
  updateWindow (sidebarWindow displayState) $ do
    clear
    forM_ lines $ \(screenIndex, lineNumber, tokens) -> do
      moveCursor (toInteger screenIndex) 0
      drawString $ show lineNumber

drawToken :: DisplayState -> Token -> Update ()
drawToken displayState (tokenType, tokenText) =
  let (color, attributes) =
        M.findWithDefault (defaultColorID, []) tokenType $ style displayState
   in do setColor color
         setAttributes attributes
         drawText tokenText

drawLine :: DisplayState -> NumberedLine -> Update ()
drawLine displayState (screenIndex, lineNumber, tokens) = do
  moveCursor (toInteger screenIndex) 0
  forM_ tokens $ drawToken displayState

drawBuffer :: DisplayState -> [NumberedLine] -> Curses ()
drawBuffer displayState lines =
  let updateBuffer :: Update () -> Curses ()
      updateBuffer = updateWindow $ bufferWindow displayState
      drawLine' :: NumberedLine -> Curses (Either CursesException ())
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
      highlightedBuffer :: [SourceLine]
      highlightedBuffer = bufferHighlighted $ stateBuffer state
      scrolledBuffer :: [SourceLine]
      scrolledBuffer =
        take (fromIntegral rows) $ drop rowOffset highlightedBuffer
      lines :: [NumberedLine]
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
