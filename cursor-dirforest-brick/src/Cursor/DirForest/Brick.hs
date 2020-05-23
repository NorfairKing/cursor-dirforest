{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.DirForest.Brick
  ( verticalDirForestCursorWidget,
    dirForestCursorWidget,
  )
where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.DirForest
import Cursor.Tree
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..), DirTree (..))
import Data.Int
import qualified Data.Map as M
import Graphics.Vty.Input.Events

verticalDirForestCursorWidget ::
  (CTree (FileOrDir b) -> Widget n) ->
  (TreeCursor (FileOrDir a) (FileOrDir b) -> Widget n) ->
  (CTree (FileOrDir b) -> Widget n) ->
  DirForestCursor a b ->
  Widget n
verticalDirForestCursorWidget beforeFunc currentFunc afterFunc = dirForestCursorWidget $ \befores current afters ->
  vBox $
    concat
      [ map beforeFunc befores,
        [currentFunc current],
        map afterFunc afters
      ]

dirForestCursorWidget :: ([CTree (FileOrDir b)] -> TreeCursor (FileOrDir a) (FileOrDir b) -> [CTree (FileOrDir b)] -> Widget n) -> DirForestCursor a b -> Widget n
dirForestCursorWidget = foldDirForestCursor
