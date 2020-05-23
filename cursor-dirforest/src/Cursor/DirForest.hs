{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cursor.DirForest
  ( -- * Types
    DirForestCursor (..),
    FileOrDir (..),

    -- * Construction and deconstruction

    -- ** Make
    makeDirForestCursor,

    -- ** Rebuild
    rebuildDirForestCursor,
  )
where

--     -- ** Lenses
--     dirForestCursorMapCursorL,
--
--
--     -- ** Fold
--     foldDirForestCursor,
--
--     -- * Movements
--     dirForestCursorSelectPrevOnSameLevel,
--     dirForestCursorSelectNextOnSameLevel,
--     dirForestCursorSelectFirstOnSameLevel,
--     dirForestCursorSelectLastOnSameLevel,
--     dirForestCursorSelectPrevTreeOnSameLevel,
--     dirForestCursorSelectNextTreeOnSameLevel,
--     dirForestCursorSelectFirstTreeOnSameLevel,
--     dirForestCursorSelectLastTreeOnSameLevel,
--     dirForestCursorSelectFirstChild,
--     dirForestCursorSelectLastChild,
--     dirForestCursorSelectParent,

import Control.DeepSeq
import Control.Monad
import Cursor.Forest
import Cursor.List.NonEmpty
import Cursor.Tree
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Tree
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Path.Internal
import qualified System.FilePath as FP

-- | A cursor for a dirforest.
--
-- This cursor is necessarily nonempty.
-- If you need a dirforestcursor that can be empty, wrap it in a Maybe
--
-- A user can look at any file or directory in the forest.
--
-- Internally this cursor is represented as a forest cursor of (either file (with contents) or directory (without contents)).
-- This means that files must not have children.
newtype DirForestCursor a b = DirForestCursor {dirForestCursorForestCursor :: ForestCursor (FileOrDir a) (FileOrDir b)}
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (DirForestCursor a b) where
  validate dfc =
    mconcat
      [ genericValidate dfc,
        decorate "The forest cursor" $
          let fc = dirForestCursorForestCursor dfc
           in decorate "The nonempty cursor" $
                let nec = forestCursorListCursor fc
                    goTree :: Tree (FileOrDir s) -> Validation
                    goTree (Node a f) =
                      mconcat
                        [ declare "If the node is a file, then the forest below is empty" $
                            case a of
                              FodFile _ _ -> case f of
                                [] -> True
                                _ -> False
                              FodDir _ -> True,
                          decorate "The Forest below" $ goForest f
                        ]
                    goForest :: Forest (FileOrDir s) -> Validation
                    goForest f = decorateList f goTree
                    goCTree :: CTree (FileOrDir s) -> Validation
                    goCTree (CNode a cf) =
                      mconcat
                        [ declare "If the node is a file, then the forest below is empty" $
                            case a of
                              FodFile _ _ -> case cf of
                                EmptyCForest -> True
                                _ -> False
                              FodDir _ -> True,
                          decorate "The CForest below" $ goCForest cf
                        ]
                    goCForest :: CForest (FileOrDir s) -> Validation
                    goCForest = \case
                      EmptyCForest -> mempty
                      ClosedForest net -> decorateList (NE.toList net) goTree
                      OpenForest nect -> decorateList (NE.toList nect) goCTree
                 in mconcat
                      [ decorate "The prev trees" $ mconcat $ map goCTree $ nonEmptyCursorPrev nec,
                        decorate "The current tree" $
                          let tc = nonEmptyCursorCurrent nec
                              goTreeAbove :: TreeAbove (FileOrDir s) -> Validation
                              goTreeAbove ta =
                                mconcat
                                  [ decorate "The treeAboveLefts" $ decorateList (treeAboveLefts ta) goCTree,
                                    decorate "The treeAboveRights" $ decorateList (treeAboveRights ta) goCTree,
                                    declare "The treeAboveNode is a dir" $ case treeAboveNode ta of
                                      FodDir _ -> True
                                      _ -> False,
                                    decorate "The treeAboveAbove recursively" $ maybe mempty goTreeAbove $ treeAboveAbove ta
                                  ]
                           in mconcat
                                [ decorate "The treeAbove" $ maybe mempty goTreeAbove $ treeAbove tc,
                                  declare "If the currently selected node is a file, then the forest below is empty" $
                                    case treeCurrent tc of
                                      FodFile _ _ -> case treeBelow tc of
                                        EmptyCForest -> True
                                        _ -> False
                                      FodDir _ -> True,
                                  decorate "The treeBelow" $ goCForest $ treeBelow tc
                                ],
                        decorate "The next trees" $ mconcat $ map goCTree $ nonEmptyCursorNext nec
                        -- TODO the names need to be unique
                      ]
      ]

instance (NFData a, NFData b) => NFData (DirForestCursor a b)

data FileOrDir a = FodFile (Path Rel File) a | FodDir (Path Rel Dir)
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (FileOrDir a) where
  validate fod =
    let isTopLevel p_ = parent p_ == [reldir|./|]
     in mconcat
          [ genericValidate fod,
            declare "The path is toplevel" $ case fod of
              FodFile rf _ -> isTopLevel rf
              FodDir rd -> isTopLevel rd
          ]

instance NFData a => NFData (FileOrDir a)

--
-- dirForestCursorMapCursorL :: Lens' (DirForestCursor a) (MapCursor FilePath (DirForestCursor a) FilePath (DirTree a))
-- dirForestCursorMapCursorL = lens dirForestCursorMapCursor $ \dfc mc -> dfc {dirForestCursorMapCursor = mc}

-- | Make a 'DirForestCursor'.
--
-- This will fail if the dirforest is empty.
makeDirForestCursor :: (b -> a) -> DirForest b -> Maybe (DirForestCursor a b)
makeDirForestCursor func = fmap (DirForestCursor . makeForestCursor (fmap func) . NE.map (cTree True)) . NE.nonEmpty . toForest
  where
    toForest :: DirForest b -> Forest (FileOrDir b)
    toForest = goDF
      where
        goDF :: DirForest b -> Forest (FileOrDir b)
        goDF = map (uncurry goDT) . M.toList . unDirForest
        goDT :: FilePath -> DirTree b -> Tree (FileOrDir b)
        goDT f dt = case dt of
          NodeFile v -> Node (FodFile (Path f) v) []
          NodeDir df -> Node (FodDir (Path $ FP.addTrailingPathSeparator f)) (goDF df)

rebuildDirForestCursor :: (a -> b) -> DirForestCursor a b -> DirForest b
rebuildDirForestCursor func = fromForest . NE.toList . NE.map rebuildCTree . rebuildForestCursor (fmap func) . dirForestCursorForestCursor
  where
    fromForest :: Forest (FileOrDir b) -> DirForest b
    fromForest = goF
      where
        goF :: Forest (FileOrDir b) -> DirForest b
        goF = DirForest . M.fromList . map goT
        goT :: Tree (FileOrDir b) -> (FilePath, DirTree b)
        goT (Node fod f) = case fod of
          FodFile rf v -> (fromRelFile rf, NodeFile v)
          FodDir rd -> (FP.dropTrailingPathSeparator $ fromRelDir rd, NodeDir $ goF f)
-- foldDirForestCursor :: ([(FilePath, DirTree a)] -> KeyValueCursor FilePath (DirForestCursor a) FilePath (DirTree a) -> [(FilePath, DirTree a)] -> c) -> DirForestCursor a -> c
-- foldDirForestCursor func (DirForestCursor m) = foldMapCursor func m
--
-- dirForestCursorSelectPrevOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectPrevOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
--   KeyValueCursorKey _ _ -> dirForestCursorSelectPrevTreeOnSameLevel dfc
--   KeyValueCursorValue fp dfc' -> do
--     dfc'' <- dirForestCursorSelectPrevOnSameLevel dfc'
--     let kvc' = KeyValueCursorValue fp dfc''
--     pure $ dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'
--
-- dirForestCursorSelectNextOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectNextOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
--   KeyValueCursorKey _ _ -> dirForestCursorSelectNextTreeOnSameLevel dfc
--   KeyValueCursorValue fp dfc' -> do
--     dfc'' <- dirForestCursorSelectNextOnSameLevel dfc'
--     let kvc' = KeyValueCursorValue fp dfc''
--     pure $ dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'
--
-- dirForestCursorSelectFirstOnSameLevel :: DirForestCursor a -> DirForestCursor a
-- dirForestCursorSelectFirstOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
--   KeyValueCursorKey _ _ -> dirForestCursorSelectFirstTreeOnSameLevel dfc
--   KeyValueCursorValue fp dfc' ->
--     let kvc' = KeyValueCursorValue fp $ dirForestCursorSelectFirstOnSameLevel dfc'
--      in dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'
--
-- dirForestCursorSelectLastOnSameLevel :: DirForestCursor a -> DirForestCursor a
-- dirForestCursorSelectLastOnSameLevel dfc = case dfc ^. dirForestCursorMapCursorL . mapCursorElemL of
--   KeyValueCursorKey _ _ -> dirForestCursorSelectLastTreeOnSameLevel dfc
--   KeyValueCursorValue fp dfc' ->
--     let kvc' = KeyValueCursorValue fp $ dirForestCursorSelectLastOnSameLevel dfc'
--      in dfc & dirForestCursorMapCursorL . mapCursorElemL .~ kvc'
--
-- dirForestCursorSelectPrevTreeOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectPrevTreeOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectPrev rebuildKeyCursor makeKeyCursor rebuildValueCursor
--
-- dirForestCursorSelectNextTreeOnSameLevel :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectNextTreeOnSameLevel = dirForestCursorMapCursorL $ mapCursorSelectNext rebuildKeyCursor makeKeyCursor rebuildValueCursor
--
-- dirForestCursorSelectFirstTreeOnSameLevel :: DirForestCursor a -> DirForestCursor a
-- dirForestCursorSelectFirstTreeOnSameLevel = dirForestCursorMapCursorL %~ mapCursorSelectFirst rebuildKeyCursor makeKeyCursor rebuildValueCursor
--
-- dirForestCursorSelectLastTreeOnSameLevel :: DirForestCursor a -> DirForestCursor a
-- dirForestCursorSelectLastTreeOnSameLevel = dirForestCursorMapCursorL %~ mapCursorSelectLast rebuildKeyCursor makeKeyCursor rebuildValueCursor
--
-- dirForestCursorSelectFirstChild :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectFirstChild = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
--   KeyValueCursorKey fp dt -> case dt of
--     NodeFile _ -> Nothing
--     NodeDir df -> KeyValueCursorValue (makeKeyCursor fp) . dirForestCursorSelectFirstOnSameLevel <$> makeDirForestCursor df
--   KeyValueCursorValue fp dfc ->
--     KeyValueCursorValue fp <$> dirForestCursorSelectFirstChild dfc
--
-- dirForestCursorSelectLastChild :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectLastChild = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
--   KeyValueCursorKey fp dt -> case dt of
--     NodeFile _ -> Nothing
--     NodeDir df -> KeyValueCursorValue (makeKeyCursor fp) . dirForestCursorSelectLastOnSameLevel <$> makeDirForestCursor df
--   KeyValueCursorValue fp dfc ->
--     KeyValueCursorValue fp <$> dirForestCursorSelectFirstChild dfc
--
-- dirForestCursorSelectParent :: DirForestCursor a -> Maybe (DirForestCursor a)
-- dirForestCursorSelectParent = dirForestCursorMapCursorL . mapCursorElemL $ \kvc -> case kvc of
--   KeyValueCursorKey _ _ -> Just kvc
--   KeyValueCursorValue fp dfc -> undefined
--
-- makeKeyCursor :: FilePath -> FilePath
-- makeKeyCursor = id
--
-- rebuildKeyCursor :: FilePath -> FilePath
-- rebuildKeyCursor = id
--
-- rebuildValueCursor :: DirForestCursor a -> DirTree a
-- rebuildValueCursor = NodeDir . rebuildDirForestCursor
