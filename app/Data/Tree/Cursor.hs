{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Tree.Cursor
  ( TreeCursor
  , toTree
  , toCursor
  , selection
  , CursorRelativity(..)
  , foldCursor
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  ) where

import Data.Tree ( Tree(Node, rootLabel, subForest), foldTree )

import Lens.Micro ( Lens', (^.), (&), (%~), (.~), (?~), lens )
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

splice :: TreeAbove a -> Tree a -> [ Tree a ]
splice above' centre = (above' ^. lefts) ++ [ centre ] ++ (above' ^. rights)

toTree :: forall a. TreeCursor a -> Tree a
toTree cursor = wrapAbove (cursor ^. above) (cursor ^. selection)
  where wrapAbove :: Maybe (TreeAbove a) -> Tree a -> Tree a
        wrapAbove Nothing centre = centre
        wrapAbove (Just above') centre = wrapAbove (above' ^. parent)
          $ Node (above' ^. self)
          $ splice above' centre

data CursorRelativity = IsTarget | InsideTarget | NotTarget

foldCursor
  :: forall a b. (CursorRelativity -> a -> [ b ] -> b) -> TreeCursor a -> b
foldCursor f cursor = foldAbove (cursor ^. above)
  $ f IsTarget (cursor ^. target)
  $ foldForest InsideTarget (cursor ^. below)
  where foldForest :: CursorRelativity -> [ Tree a ] -> [ b ]
        foldForest relativity = map $ foldTree $ f relativity

        foldSplice :: TreeAbove a -> b -> [ b ]
        foldSplice above' centre = (foldForest NotTarget $ above' ^. lefts)
          ++ [ centre ]
          ++ (foldForest NotTarget $ above' ^. rights)

        foldAbove :: Maybe (TreeAbove a) -> b -> b
        foldAbove Nothing centre = centre
        foldAbove (Just above') centre = foldAbove (above' ^. parent)
          $ f NotTarget (above' ^. self) (foldSplice above' centre)

toCursor :: Tree a -> TreeCursor a
toCursor tree = TreeCursor { _above = Nothing
                           , _target = rootLabel tree
                           , _below = subForest tree
                           }

selection :: forall a. Lens' (TreeCursor a) (Tree a)
selection = lens getSelection setSelection
  where getSelection :: TreeCursor a -> Tree a
        getSelection cursor = Node (cursor ^. target) (cursor ^. below)

        setSelection :: TreeCursor a -> Tree a -> TreeCursor a
        setSelection cursor tree
          = cursor & target .~ rootLabel tree & below .~ subForest tree

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
                     & below .~ splice above' (cursor ^. selection)

moveLeft :: TreeCursor a -> Maybe (TreeCursor a)
moveLeft cursor = do
  above' <- cursor ^. above
  lefts' <- listToMaybe $ above' ^. lefts
  let above''
        = above' & lefts .~ init lefts' & rights %~ ((cursor ^. selection) :)
  return
    $ cursor
    & above ?~ above''
    & target .~ rootLabel (last lefts')
    & below .~ subForest (last lefts')

moveRight :: TreeCursor a -> Maybe (TreeCursor a)
moveRight cursor = do
  above' <- cursor ^. above
  rights' <- listToMaybe $ above' ^. rights
  let above'' = above'
        & lefts %~ (++ [ cursor ^. selection ])
        & rights .~ tail rights'
  return
    $ cursor
    & above ?~ above''
    & target .~ rootLabel (head rights')
    & below .~ subForest (head rights')
