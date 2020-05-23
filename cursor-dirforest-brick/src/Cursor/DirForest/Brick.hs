{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.DirForest.Brick
  ( verticalDirForestCursorWidget,
    dirForestCursorWidget,
    dirTreeCursorWidget,
  )
where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.DirForest
import Cursor.Map
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..), DirTree (..))
import Data.Int
import qualified Data.Map as M
import Graphics.Vty.Input.Events

verticalDirForestCursorWidget ::
  (FilePath -> DirTree a -> Widget n) ->
  (KeyValueCursor FilePath (DirTreeCursor a) FilePath (DirTree a) -> Widget n) ->
  (FilePath -> DirTree a -> Widget n) ->
  DirForestCursor a ->
  Widget n
verticalDirForestCursorWidget beforeFunc currentFunc afterFunc = dirForestCursorWidget $ \befores current afters ->
  vBox $
    concat
      [ map (uncurry beforeFunc) befores,
        [currentFunc current],
        map (uncurry afterFunc) afters
      ]

dirForestCursorWidget :: ([(FilePath, DirTree a)] -> KeyValueCursor FilePath (DirTreeCursor a) FilePath (DirTree a) -> [(FilePath, DirTree a)] -> Widget n) -> DirForestCursor a -> Widget n
dirForestCursorWidget = foldDirForestCursor

dirTreeCursorWidget :: (a -> Widget n) -> (Maybe (DirForestCursor a) -> Widget n) -> (DirTreeCursor a -> Widget n)
dirTreeCursorWidget = foldDirTreeCursor
