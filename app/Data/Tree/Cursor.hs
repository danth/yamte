{-# LANGUAGE TemplateHaskell #-}

module Data.Tree.Cursor
  ( TreeCursor
  , toTree
  , toCursor
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  ) where

import Data.Tree ( Tree(Node, rootLabel, subForest) )

import Lens.Micro ( (^.), (&), (%~), (.~), (?~) )
import Lens.Micro.TH ( makeLenses )

data TreeAbove a = TreeAbove
  { _parent :: Maybe (TreeAbove a)
  , _self :: a
  , _lefts :: [ Tree a ]
  , _rights :: [ Tree a ]
  }

data TreeCursor a = TreeCursor
  { _above :: Maybe (TreeAbove a)
  , _target :: a
  , _below :: [ Tree a ]
  }

makeLenses ''TreeAbove

makeLenses ''TreeCursor

cursorToNode :: TreeCursor a -> Tree a
cursorToNode cursor = Node (cursor ^. target) (cursor ^. below)

splice :: TreeAbove a -> Tree a -> [ Tree a ]
splice above' centre = (above' ^. lefts) ++ [ centre ] ++ (above' ^. rights)

toTree :: TreeCursor a -> Tree a
toTree cursor = wrapAbove (cursor ^. above) $ cursorToNode cursor
  where wrapAbove :: Maybe (TreeAbove a) -> Tree a -> Tree a
        wrapAbove Nothing centre = centre
        wrapAbove (Just above') centre = wrapAbove (above' ^. parent)
          $ Node (above' ^. self)
          $ splice above' centre

toCursor :: Tree a -> TreeCursor a
toCursor tree = TreeCursor { _above = Nothing
                           , _target = rootLabel tree
                           , _below = subForest tree
                           }

listToMaybe :: [ a ] -> Maybe [ a ]
listToMaybe [] = Nothing
listToMaybe as = Just as

moveDown :: TreeCursor a -> Maybe (TreeCursor a)
moveDown cursor = do
  children <- listToMaybe $ cursor ^. below
  return
    $ cursor
    & above
    ?~ TreeAbove { _parent = cursor ^. above
                 , _self = cursor ^. target
                 , _lefts = []
                 , _rights = tail children
                 }
    & target .~ rootLabel (head children)
    & below .~ subForest (head children)

moveUp :: TreeCursor a -> Maybe (TreeCursor a)
moveUp cursor = do above' <- cursor ^. above
                   return
                     $ cursor
                     & above .~ (above' ^. parent)
                     & target .~ (above' ^. self)
                     & below .~ splice above' (cursorToNode cursor)

moveLeft :: TreeCursor a -> Maybe (TreeCursor a)
moveLeft cursor = do
  above' <- cursor ^. above
  lefts' <- listToMaybe $ above' ^. lefts
  let above''
        = above' & lefts .~ init lefts' & rights %~ (cursorToNode cursor :)
  return
    $ cursor
    & above ?~ above''
    & target .~ rootLabel (last lefts')
    & below .~ subForest (last lefts')

moveRight :: TreeCursor a -> Maybe (TreeCursor a)
moveRight cursor = do
  above' <- cursor ^. above
  rights' <- listToMaybe $ above' ^. rights
  let above''
        = above' & lefts %~ (cursorToNode cursor :) & rights .~ tail rights'
  return
    $ cursor
    & above ?~ above''
    & target .~ rootLabel (head rights')
    & below .~ subForest (head rights')
