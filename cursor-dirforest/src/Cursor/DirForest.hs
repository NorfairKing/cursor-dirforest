{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cursor.DirForest
  ( -- * Types
    DirForestCursor (..),
    DirTreeCursor (..),

    -- ** Lenses
    dirForestCursorMapCursorL,

    -- * Construction and deconstruction

    -- ** Make
    makeDirForestCursor,
    makeDirTreeCursor,

    -- ** Rebuild
    rebuildDirForestCursor,
    rebuildDirTreeCursor,

    -- ** Fold
    foldDirForestCursor,
    foldDirTreeCursor,

    -- * Movements
    dirForestCursorSelectPrevOnSameLevel,
    dirForestCursorSelectNextOnSameLevel,
    dirForestCursorSelectFirstChild,
    dirForestCursorSelectLastChild,
    dirTreeCursorSelectFirstChild,
    dirTreeCursorSelectLastChild,
  )
where

import Control.DeepSeq
import Control.Monad
import Cursor.Map
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

-- | A cursor for a dirforest
--
-- A user can look at any file or directory in the forest.
--
-- Internally this cursor is represented as a nonempty map cursor of paths and directory trees or directory tree cursors.
-- When, inside the KeyValueCursor, the key is selected, that means that that filepath is selected.
-- When the value is selected, that means that the some file or dir below is selected.
newtype DirForestCursor a = DirForestCursor {dirForestCursorMapCursor :: MapCursor FilePath (Maybe (DirForestCursor a)) FilePath (DirTree a)}
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (DirForestCursor a) where
  validate dfc@(DirForestCursor mc) =
    mconcat
      [ genericValidate dfc,
        delve "it can be rebuilt to a valid dirforest" $ rebuildDirForestCursor dfc
      ]

instance (NFData a, Ord a) => NFData (DirForestCursor a)

data DirTreeCursor a
  = DirTreeCursorFile a
  | DirTreeCursorDir (Maybe (DirForestCursor a)) -- Nothing means an empty dir
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (DirTreeCursor a)

instance (NFData a, Ord a) => NFData (DirTreeCursor a)

dirForestCursorMapCursorL :: Lens' (DirForestCursor a) (MapCursor FilePath (DirTreeCursor a) FilePath (DirTree a))
dirForestCursorMapCursorL = lens dirForestCursorMapCursor $ \dfc mc -> dfc {dirForestCursorMapCursor = mc}

makeDirForestCursor :: DirForest a -> Maybe (DirForestCursor a)
makeDirForestCursor (DirForest m) = fmap DirForestCursor $ makeMapCursor id <$> NE.nonEmpty (M.toList m)

rebuildDirForestCursor :: DirForestCursor a -> DirForest a
rebuildDirForestCursor (DirForestCursor mc) = DirForest $ M.fromList $ NE.toList $ rebuildMapCursor id rebuildDirTreeCursor mc

makeDirTreeCursor :: DirTree a -> DirTreeCursor a
makeDirTreeCursor = \case
  NodeFile a -> DirTreeCursorFile a
  NodeDir df -> DirTreeCursorDir $ makeDirForestCursor df -- Nested forests can't be empty

rebuildDirTreeCursor :: DirTreeCursor a -> DirTree a
rebuildDirTreeCursor = \case
  DirTreeCursorFile a -> NodeFile a
  DirTreeCursorDir dfc -> NodeDir $ maybe DF.empty rebuildDirForestCursor dfc

foldDirForestCursor :: ([(FilePath, DirTree a)] -> KeyValueCursor FilePath (DirTreeCursor a) FilePath (DirTree a) -> [(FilePath, DirTree a)] -> c) -> DirForestCursor a -> c
foldDirForestCursor func (DirForestCursor m) = foldMapCursor func m

foldDirTreeCursor :: (a -> b) -> (Maybe (DirForestCursor a) -> b) -> DirTreeCursor a -> b
foldDirTreeCursor fileFunc dirFunc = \case
  DirTreeCursorFile a -> fileFunc a
  DirTreeCursorDir mdf -> dirFunc mdf

dirForestCursorSelectPrevOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrevOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectPrev rebuildKeyCursor makeKeyCursor rebuildDirTreeCursor

dirForestCursorSelectNextOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNextOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectNext rebuildKeyCursor makeKeyCursor rebuildDirTreeCursor

dirForestCursorSelectFirstChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectFirstChild =
  dirForestCursorMapCursorL $
    (mapCursorTraverseValueCase (\fp dtc -> (,) fp <$> dirTreeCursorSelectFirstChild dtc))
      . (mapCursorSelectValue rebuildKeyCursor makeDirTreeCursor)

dirForestCursorSelectLastChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectLastChild =
  dirForestCursorMapCursorL $
    (mapCursorTraverseValueCase (\fp dtc -> (,) fp <$> dirTreeCursorSelectLastChild dtc))
      . (mapCursorSelectValue rebuildKeyCursor makeDirTreeCursor)

dirTreeCursorSelectFirstChild :: DirTreeCursor a -> Maybe (DirTreeCursor a)
dirTreeCursorSelectFirstChild = \case
  DirTreeCursorFile _ -> Nothing
  DirTreeCursorDir mdf -> do
    df <- mdf
    (DirTreeCursorDir . Just) <$> dirForestCursorSelectFirstChild df

dirTreeCursorSelectLastChild :: DirTreeCursor a -> Maybe (DirTreeCursor a)
dirTreeCursorSelectLastChild = \case
  DirTreeCursorFile _ -> Nothing
  DirTreeCursorDir mdf -> do
    df <- mdf
    (DirTreeCursorDir . Just) <$> dirForestCursorSelectLastChild df

makeKeyCursor :: FilePath -> FilePath
makeKeyCursor = id

rebuildKeyCursor :: FilePath -> FilePath
rebuildKeyCursor = id
