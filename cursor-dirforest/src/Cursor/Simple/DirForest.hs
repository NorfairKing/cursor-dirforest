{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cursor.Simple.DirForest
  ( -- * Types
    DirForestCursor,
    DFC.dirForestCursorForestCursor,
    DFC.FileOrDir (..),

    -- * Construction and deconstruction

    -- ** Make
    makeDirForestCursor,

    -- ** Rebuild
    rebuildDirForestCursor,
    DFC.isTopLevel,

    -- ** Lenses
    DFC.dirForestCursorForestCursorL,
    DFC.dirForestCursorSelectedL,

    -- ** Fold
    DFC.foldDirForestCursor,

    -- * Query
    DFC.dirForestCursorSelected,

    -- * Movements
    dirForestCursorSelectPrevOnSameLevel,
    dirForestCursorSelectNextOnSameLevel,
    dirForestCursorSelectFirstOnSameLevel,
    dirForestCursorSelectLastOnSameLevel,
    dirForestCursorSelectPrevTree,
    dirForestCursorSelectNextTree,
    dirForestCursorSelectFirstTree,
    dirForestCursorSelectLastTree,
    dirForestCursorSelectPrev,
    dirForestCursorSelectNext,
    dirForestCursorSelectFirst,
    dirForestCursorSelectLast,
    dirForestCursorSelectFirstChild,
    dirForestCursorSelectLastChild,
    dirForestCursorSelectParent,

    -- * Collapsing

    -- ** One level
    DFC.dirForestCursorOpen,
    DFC.dirForestCursorClose,
    DFC.dirForestCursorToggle,

    -- ** Recursively
    DFC.dirForestCursorOpenRecursively,
    DFC.dirForestCursorToggleRecursively,
  )
where

import qualified Cursor.DirForest as DFC
import Data.DirForest (DirForest (..))

type DirForestCursor a = DFC.DirForestCursor a a

-- | Make a 'DirForestCursor'.
--
-- This will fail if the dirforest is empty.
makeDirForestCursor :: DirForest a -> Maybe (DirForestCursor a)
makeDirForestCursor = DFC.makeDirForestCursor id

rebuildDirForestCursor :: DirForestCursor a -> DirForest a
rebuildDirForestCursor = DFC.rebuildDirForestCursor id

dirForestCursorSelectPrevOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrevOnSameLevel = DFC.dirForestCursorSelectPrevOnSameLevel id id

dirForestCursorSelectNextOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNextOnSameLevel = DFC.dirForestCursorSelectNextOnSameLevel id id

dirForestCursorSelectFirstOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectFirstOnSameLevel = DFC.dirForestCursorSelectFirstOnSameLevel id id

dirForestCursorSelectLastOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectLastOnSameLevel = DFC.dirForestCursorSelectLastOnSameLevel id id

dirForestCursorSelectPrevTree :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrevTree = DFC.dirForestCursorSelectPrevTree id id

dirForestCursorSelectNextTree :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNextTree = DFC.dirForestCursorSelectNextTree id id

dirForestCursorSelectFirstTree :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectFirstTree = DFC.dirForestCursorSelectFirstTree id id

dirForestCursorSelectLastTree :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectLastTree = DFC.dirForestCursorSelectLastTree id id

dirForestCursorSelectPrev :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrev = DFC.dirForestCursorSelectPrev id id

dirForestCursorSelectNext :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNext = DFC.dirForestCursorSelectNext id id

dirForestCursorSelectFirst :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectFirst = DFC.dirForestCursorSelectFirst id id

dirForestCursorSelectLast :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectLast = DFC.dirForestCursorSelectLast id id

dirForestCursorSelectFirstChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectFirstChild = DFC.dirForestCursorSelectFirstChild id id

dirForestCursorSelectLastChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectLastChild = DFC.dirForestCursorSelectLastChild id id

dirForestCursorSelectParent :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectParent = DFC.dirForestCursorSelectParent id id
