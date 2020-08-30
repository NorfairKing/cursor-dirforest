{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.DirForest.Brick
  ( verticalPaddedDirForestCursorWidget,
    verticalDirForestCursorWidget,
    dirForestCursorWidget,
  )
where

import Brick.Types
import Brick.Widgets.Core
import Cursor.Brick.Forest
import Cursor.DirForest
import Cursor.FileOrDir
import Cursor.Forest
import Cursor.Tree

verticalPaddedDirForestCursorWidget ::
  (FileOrDirCursor a -> Widget n) ->
  (FileOrDir b -> Widget n) ->
  Int ->
  DirForestCursor a b ->
  Widget n
verticalPaddedDirForestCursorWidget goA goB padding =
  verticalPaddedForestCursorWidget goA goB padding . dirForestCursorForestCursor

verticalDirForestCursorWidget ::
  (CTree (FileOrDir b) -> Widget n) ->
  (TreeCursor (FileOrDirCursor a) (FileOrDir b) -> Widget n) ->
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

dirForestCursorWidget ::
  ( [CTree (FileOrDir b)] ->
    TreeCursor (FileOrDirCursor a) (FileOrDir b) ->
    [CTree (FileOrDir b)] ->
    Widget n
  ) ->
  DirForestCursor a b ->
  Widget n
dirForestCursorWidget = foldDirForestCursor
