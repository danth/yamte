module Yamte.Mode
  ( activeMode
  , enterMode
  , leaveMode
  , standardActions
  , handleEvent
  ) where

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent) )

import Control.Monad ( (<=<) )
import Control.Monad.IO.Class ( liftIO )

import Data.List ( find )
import Data.Stack ( Stack, stackIsEmpty, stackPeek, stackPop, stackPush )

import Graphics.Vty ( Event(EvKey), Key(KChar), Modifier(MCtrl) )

import Lens.Micro ( (%~), (&), (^.) )

import Yamte.Types
  ( Action(..)
  , trigger
  , transformation
  , Event'
  , EventM'
  , Mode(..)
  , ModeResponse(..)
  , ModifiedKey(..)
  , State
  , modeStack
  )

activeMode :: State -> Maybe Mode
activeMode state = stackPeek $ state ^. modeStack

enterMode :: Mode -> State -> State
enterMode mode state = state & modeStack %~ (`stackPush` mode)

leaveMode :: State -> State
leaveMode state = state & modeStack %~ (\s -> maybe s fst $ stackPop s)

standardActions :: [ Action ]
standardActions = [ exitAction ]
  where exitAction :: Action
        exitAction = Action { _trigger = ModifiedKey (KChar 'q') [ MCtrl ]
                            , _description = "Exit this mode"
                            , _transformation = return . leaveMode
                            }

findAction :: [ Action ] -> ModifiedKey -> Maybe Action
findAction actions key = find (\action -> action ^. trigger == key) actions

runAction :: Maybe Action -> State -> IO ModeResponse
runAction Nothing = const $ return DoNothing
runAction (Just action) = return . NewState <=< action ^. transformation

handleKey :: Mode -> ModifiedKey -> State -> IO ModeResponse
handleKey (FunctionMode _ f) = f
handleKey (ActionMode _ actions) = runAction . findAction actions

handleKey' :: Stack Mode -> ModifiedKey -> State -> IO State
handleKey' stack key state =
  case stackPop stack of
    Nothing -> return state
    Just (stack', mode) ->
      let handleResponse :: ModeResponse -> IO State
          handleResponse (NewState state') = return state'
          handleResponse Propagate = handleKey' stack' key state
          handleResponse DoNothing = return state
       in handleKey mode key state >>= handleResponse

handleEvent :: State -> Event' -> EventM'
handleEvent state (VtyEvent (EvKey key modifiers)) = do
  let key' = ModifiedKey key modifiers
  state' <- liftIO $ handleKey' (state ^. modeStack) key' state
  if stackIsEmpty $ state' ^. modeStack
     then halt state'
     else continue state'
handleEvent state _ = continue state
