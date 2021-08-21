module Yamte.Mode
  ( standardActions
  , handleEvent
  ) where

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent), EventM )

import Control.Monad ( (<=<) )
import Control.Monad.IO.Class ( liftIO )

import Data.List ( find )

import Graphics.Vty ( Event(EvKey), Key(KChar), Modifier(MCtrl) )

import Lens.Micro ( (%~), (&), (.~), (^.) )

import Yamte.Editor ( leaveMode )
import Yamte.Types
  ( Action(..)
  , trigger
  , description
  , transformation
  , Event'
  , EventM'
  , Mode(..)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State
  , modes
  , showHints
  )

standardActions :: [ Action ]
standardActions = [ hintAction, exitAction ]
  where hintAction :: Action
        hintAction = Action
          { _trigger = ModifiedKey (KChar '?') []
          , _description = "Toggle hints"
          , _transformation = return . (showHints %~ not)
          }

        exitAction :: Action
        exitAction = Action
          { _trigger = ModifiedKey (KChar 'q') [ MCtrl ]
          , _description = "Exit this mode"
          , _transformation = return . leaveMode
          }

findAction :: [ Action ] -> ModifiedKey -> Maybe Action
findAction actions key = find (\action -> action ^. trigger == key)
  actions

runAction :: Maybe Action -> State -> IO ModeResponse
runAction Nothing = const $ return DoNothing
runAction (Just action) = return . NewState <=< action ^. transformation

handleKey :: Mode -> ModifiedKey -> State -> IO ModeResponse
handleKey (FunctionMode _ f) = f
handleKey (ActionMode _ actions) = runAction . findAction actions

handleKey' :: [ Mode ] -> ModifiedKey -> State -> IO State
handleKey' [] key state = return state
handleKey' (mode : modes) key state = do
  response <- handleKey mode key state
  case response of NewState state' -> return state'
                   Propagate -> handleKey' modes key state
                   DoNothing -> return state

handleEvent :: State -> Event' -> EventM'
handleEvent state (VtyEvent (EvKey key modifiers)) = do
  let key' = ModifiedKey key modifiers
  state' <- liftIO $ handleKey' (state ^. modes) key' state
  case state' ^. modes of [] -> halt state'
                          _ -> continue state'
handleEvent state _ = continue state
