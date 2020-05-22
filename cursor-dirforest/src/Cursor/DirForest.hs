{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cursor.DirForest where

import Cursor.Map
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)

newtype DirForestCursor a = DirForestCursor {dirForestCursorMapCursor :: MapCursor FilePath (DirTreeCursor a) FilePath (DirTree a)}
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (DirForestCursor a) where
  validate dfc = mconcat [genericValidate dfc, delve "it can be rebuilt to a valid dirforest" $ rebuildDirForestCursor dfc]

data DirTreeCursor a
  = DirTreeCursorFile a
  | DirTreeCursorDir (DirForestCursor a)
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (DirTreeCursor a)

makeDirForestCursor :: DirForest a -> Maybe (DirForestCursor a)
makeDirForestCursor (DirForest m) = fmap DirForestCursor $ makeMapCursor id <$> NE.nonEmpty (M.toList m)

rebuildDirForestCursor :: DirForestCursor a -> DirForest a
rebuildDirForestCursor (DirForestCursor mc) = DirForest $ M.fromList $ NE.toList $ rebuildMapCursor id rebuildDirTreeCursor mc

makeDirTreeCursor :: DirTree a -> DirTreeCursor a
makeDirTreeCursor = \case
  NodeFile a -> DirTreeCursorFile a
  NodeDir df -> DirTreeCursorDir $ fromJust $ makeDirForestCursor df -- Nested forests can't be empty

rebuildDirTreeCursor :: DirTreeCursor a -> DirTree a
rebuildDirTreeCursor = \case
  DirTreeCursorFile a -> NodeFile a
  DirTreeCursorDir dfc -> NodeDir $ rebuildDirForestCursor dfc
