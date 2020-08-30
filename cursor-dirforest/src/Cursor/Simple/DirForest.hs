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

    -- *** Helper
    dirForestCursorPrepareForMovement,

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
    DFC.dirForestCursorSelectPrevChar,
    DFC.dirForestCursorSelectNextChar,

    -- * Edits
    dirForestCursorDeleteCurrent,
    dirForestCursorStartNew,
    DFC.dirForestCursorInsertChar,
    DFC.dirForestCursorAppendChar,
    DFC.dirForestCursorRemoveChar,
    DFC.dirForestCursorDeleteChar,

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
import Cursor.Simple.Forest
import Cursor.Types
import Data.DirForest (DirForest (..))

type DirForestCursor a = DFC.DirForestCursor a a

-- | Make a 'DirForestCursor'.
--
-- This will fail if the dirforest is empty.
makeDirForestCursor :: DirForest a -> Maybe (DirForestCursor a)
makeDirForestCursor = DFC.makeDirForestCursor id

dirForestCursorPrepareForMovement :: DirForestCursor a -> DeleteOrUpdate (ForestCursor (DFC.FileOrDir a))
dirForestCursorPrepareForMovement = DFC.dirForestCursorPrepareForMovement id

rebuildDirForestCursor :: DirForestCursor a -> DeleteOrUpdate (DirForest a)
rebuildDirForestCursor = DFC.rebuildDirForestCursor id id

dirForestCursorSelectPrevOnSameLevel :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectPrevOnSameLevel = DFC.dirForestCursorSelectPrevOnSameLevel id id

dirForestCursorSelectNextOnSameLevel :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectNextOnSameLevel = DFC.dirForestCursorSelectNextOnSameLevel id id

dirForestCursorSelectFirstOnSameLevel :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectFirstOnSameLevel = DFC.dirForestCursorSelectFirstOnSameLevel id id

dirForestCursorSelectLastOnSameLevel :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectLastOnSameLevel = DFC.dirForestCursorSelectLastOnSameLevel id id

dirForestCursorSelectPrevTree :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectPrevTree = DFC.dirForestCursorSelectPrevTree id id

dirForestCursorSelectNextTree :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectNextTree = DFC.dirForestCursorSelectNextTree id id

dirForestCursorSelectFirstTree :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectFirstTree = DFC.dirForestCursorSelectFirstTree id id

dirForestCursorSelectLastTree :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectLastTree = DFC.dirForestCursorSelectLastTree id id

dirForestCursorSelectPrev :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectPrev = DFC.dirForestCursorSelectPrev id id

dirForestCursorSelectNext :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectNext = DFC.dirForestCursorSelectNext id id

dirForestCursorSelectFirst :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectFirst = DFC.dirForestCursorSelectFirst id id

dirForestCursorSelectLast :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorSelectLast = DFC.dirForestCursorSelectLast id id

dirForestCursorSelectFirstChild :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectFirstChild = DFC.dirForestCursorSelectFirstChild id id

dirForestCursorSelectLastChild :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectLastChild = DFC.dirForestCursorSelectLastChild id id

dirForestCursorSelectParent :: DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))
dirForestCursorSelectParent = DFC.dirForestCursorSelectParent id id

dirForestCursorDeleteCurrent :: DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)
dirForestCursorDeleteCurrent = DFC.dirForestCursorDeleteCurrent id

dirForestCursorStartNew :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorStartNew = DFC.dirForestCursorStartNew id id
