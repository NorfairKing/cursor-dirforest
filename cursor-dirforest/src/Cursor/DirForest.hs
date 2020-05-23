{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cursor.DirForest
  ( -- * Types
    DirForestCursor (..),

    -- ** Lenses
    dirForestCursorMapCursorL,

    -- * Construction and deconstruction

    -- ** Make
    makeDirForestCursor,

    -- ** Rebuild
    rebuildDirForestCursor,

    -- ** Fold
    foldDirForestCursor,

    -- * Movements
    dirForestCursorSelectPrevOnSameLevel,
    dirForestCursorSelectNextOnSameLevel,
    dirForestCursorSelectFirstOnSameLevel,
    dirForestCursorSelectLastOnSameLevel,
    dirForestCursorSelectPrevTreeOnSameLevel,
    dirForestCursorSelectNextTreeOnSameLevel,
    dirForestCursorSelectFirstTreeOnSameLevel,
    dirForestCursorSelectLastTreeOnSameLevel,
    dirForestCursorSelectFirstChild,
    dirForestCursorSelectLastChild,
    dirForestCursorSelectParent,
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

-- | A cursor for a dirforest.
--
-- This cursor is necessarily nonempty.
-- If you need a dirforestcursor that can be empty, wrap it in a Maybe
--
-- A user can look at any file or directory in the forest.
--
-- Internally this cursor is represented as a nonempty map cursor of paths and directory trees or directory tree cursors.
-- When, inside the KeyValueCursor, the key is selected, that means that that filepath is selected.
-- When the value is selected, that means that the some file or dir below is selected.
newtype DirForestCursor a = DirForestCursor {dirForestCursorMapCursor :: MapCursor FilePath (DirForestCursor a) FilePath (DirTree a)}
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (DirForestCursor a) where
  validate dfc@(DirForestCursor mc) =
    mconcat
      [ genericValidate dfc,
        delve "it can be rebuilt to a valid dirforest" $ rebuildDirForestCursor dfc
      ]

instance (NFData a, Ord a) => NFData (DirForestCursor a)

dirForestCursorMapCursorL :: Lens' (DirForestCursor a) (MapCursor FilePath (DirForestCursor a) FilePath (DirTree a))
dirForestCursorMapCursorL = lens dirForestCursorMapCursor $ \dfc mc -> dfc {dirForestCursorMapCursor = mc}

makeDirForestCursor :: DirForest a -> Maybe (DirForestCursor a)
makeDirForestCursor (DirForest m) = fmap DirForestCursor $ makeMapCursor id <$> NE.nonEmpty (M.toList m)

rebuildDirForestCursor :: DirForestCursor a -> DirForest a
rebuildDirForestCursor (DirForestCursor mc) = DirForest $ M.fromList $ NE.toList $ rebuildMapCursor id rebuildValueCursor mc

foldDirForestCursor :: ([(FilePath, DirTree a)] -> KeyValueCursor FilePath (DirForestCursor a) FilePath (DirTree a) -> [(FilePath, DirTree a)] -> c) -> DirForestCursor a -> c
foldDirForestCursor func (DirForestCursor m) = foldMapCursor func m

dirForestCursorSelectPrevOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrevOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
  KeyValueCursorKey _ _ -> dirForestCursorSelectPrevTreeOnSameLevel dfc
  KeyValueCursorValue fp dfc' -> do
    dfc'' <- dirForestCursorSelectPrevOnSameLevel dfc'
    let kvc' = KeyValueCursorValue fp dfc''
    pure $ dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'

dirForestCursorSelectNextOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNextOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
  KeyValueCursorKey _ _ -> dirForestCursorSelectNextTreeOnSameLevel dfc
  KeyValueCursorValue fp dfc' -> do
    dfc'' <- dirForestCursorSelectNextOnSameLevel dfc'
    let kvc' = KeyValueCursorValue fp dfc''
    pure $ dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'

dirForestCursorSelectFirstOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectFirstOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
  KeyValueCursorKey _ _ -> dirForestCursorSelectFirstTreeOnSameLevel dfc
  KeyValueCursorValue fp dfc' ->
    let kvc' = KeyValueCursorValue fp $ dirForestCursorSelectFirstOnSameLevel dfc'
     in dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'

dirForestCursorSelectLastOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectLastOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
  KeyValueCursorKey _ _ -> dirForestCursorSelectLastTreeOnSameLevel dfc
  KeyValueCursorValue fp dfc' ->
    let kvc' = KeyValueCursorValue fp $ dirForestCursorSelectLastOnSameLevel dfc'
     in dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'

dirForestCursorSelectPrevTreeOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectPrevTreeOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectPrev rebuildKeyCursor makeKeyCursor rebuildValueCursor

dirForestCursorSelectNextTreeOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectNextTreeOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectNext rebuildKeyCursor makeKeyCursor rebuildValueCursor

dirForestCursorSelectFirstTreeOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectFirstTreeOnSameLevel = dirForestCursorMapCursorL %~ mapCursorSelectFirst rebuildKeyCursor makeKeyCursor rebuildValueCursor

dirForestCursorSelectLastTreeOnSameLevel :: DirForestCursor a -> DirForestCursor a
dirForestCursorSelectLastTreeOnSameLevel = dirForestCursorMapCursorL %~ mapCursorSelectLast rebuildKeyCursor makeKeyCursor rebuildValueCursor

dirForestCursorSelectFirstChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectFirstChild = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
  KeyValueCursorKey fp dt -> case dt of
    NodeFile _ -> Nothing
    NodeDir df -> KeyValueCursorValue (makeKeyCursor fp) . dirForestCursorSelectFirstOnSameLevel <$> makeDirForestCursor df
  KeyValueCursorValue fp dfc ->
    KeyValueCursorValue fp <$> dirForestCursorSelectFirstChild dfc

dirForestCursorSelectLastChild :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectLastChild = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
  KeyValueCursorKey fp dt -> case dt of
    NodeFile _ -> Nothing
    NodeDir df -> KeyValueCursorValue (makeKeyCursor fp) . dirForestCursorSelectLastOnSameLevel <$> makeDirForestCursor df
  KeyValueCursorValue fp dfc ->
    KeyValueCursorValue fp <$> dirForestCursorSelectFirstChild dfc

dirForestCursorSelectParent :: DirForestCursor a -> Maybe (DirForestCursor a)
dirForestCursorSelectParent = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
  KeyValueCursorKey _ _ -> Just kvc
  KeyValueCursorValue fp dfc -> undefined

makeKeyCursor :: FilePath -> FilePath
makeKeyCursor = id

rebuildKeyCursor :: FilePath -> FilePath
rebuildKeyCursor = id

rebuildValueCursor :: DirForestCursor a -> DirTree a
rebuildValueCursor = NodeDir . rebuildDirForestCursor
