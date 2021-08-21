module Yamte.Mode
  ( standardActions
  , getTrigger
  , getDescription
  , handleEvent
  ) where

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent), EventM )

import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( liftIO )

import Data.List ( find )

import Graphics.Vty ( Event(EvKey), Key(KChar), Modifier(MCtrl) )

import Lens.Micro ( (%~), (&), (^.) )

import Yamte.Editor ( leaveMode )
import Yamte.Types
  ( Action(..)
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
        hintAction = Action (ModifiedKey (KChar '?') []) "Toggle hints"
          $ \state -> state & showHints %~ not

        exitAction :: Action
        exitAction = Action (ModifiedKey (KChar 'q') [ MCtrl ]) "Exit this mode"
          leaveMode

getTrigger :: Action -> ModifiedKey
getTrigger (Action trigger _ _) = trigger
getTrigger (IOAction trigger _ _) = trigger

getDescription :: Action -> String
getDescription (Action _ description _) = description
getDescription (IOAction _ description _) = description

findAction :: [ Action ] -> ModifiedKey -> Maybe Action
findAction actions trigger = find (\action -> getTrigger action == trigger)
  actions

runAction :: Maybe Action -> State -> IO ModeResponse
runAction Nothing = const $ return DoNothing
runAction (Just (Action _ _ f)) = return . NewState . f
runAction (Just (IOAction _ _ f)) = f >=> return . NewState

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
