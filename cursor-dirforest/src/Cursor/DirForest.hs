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
    isTopLevel,

    -- ** Lenses
    dirForestCursorForestCursorL,
    dirForestCursorSelectedL,

    -- ** Fold
    foldDirForestCursor,

    -- * Query
    dirForestCursorSelected,

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

    -- * Edits
    dirForestCursorDeleteCurrent,

    -- * Collapsing

    -- ** One level
    dirForestCursorOpen,
    dirForestCursorClose,
    dirForestCursorToggle,

    -- ** Recursively
    dirForestCursorOpenRecursively,
    dirForestCursorToggleRecursively,
  )
where

import Control.DeepSeq
import Cursor.FileOrDir
import Cursor.Forest
import Cursor.List.NonEmpty
import Cursor.Tree
import Cursor.Types
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
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

dirForestCursorForestCursorL :: Lens' (DirForestCursor a b) (ForestCursor (FileOrDir a) (FileOrDir b))
dirForestCursorForestCursorL = lens dirForestCursorForestCursor $ \dfc mc -> dfc {dirForestCursorForestCursor = mc}

-- | The selected 'FileOrDir'.
--
-- Note that its path will only be the last piece, not the entire path.
-- If you need the entire piece, see 'dirForestCursorSelected' instead.
dirForestCursorSelectedL :: Lens' (DirForestCursor a b) (FileOrDir a)
dirForestCursorSelectedL = dirForestCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL

-- | Make a 'DirForestCursor'.
--
-- This will fail if the dirforest is empty.
makeDirForestCursor :: (b -> a) -> DirForest b -> Maybe (DirForestCursor a b)
makeDirForestCursor func = fmap (DirForestCursor . makeForestCursor (fmap func) . NE.map makeCTree) . NE.nonEmpty . toForest
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

foldDirForestCursor :: ([CTree (FileOrDir b)] -> TreeCursor (FileOrDir a) (FileOrDir b) -> [CTree (FileOrDir b)] -> c) -> DirForestCursor a b -> c
foldDirForestCursor func (DirForestCursor fc) = foldForestCursor func fc

dirForestCursorSelected :: DirForestCursor a b -> (Path Rel Dir, FileOrDir a)
dirForestCursorSelected dfc =
  let tc = dfc ^. dirForestCursorForestCursorL . forestCursorSelectedTreeL
      goMAbove :: (Path Rel Dir, c) -> Maybe (TreeAbove (FileOrDir b)) -> (Path Rel Dir, c)
      goMAbove fod = \case
        Nothing -> fod
        Just ta -> goAbove fod ta
      goAbove :: (Path Rel Dir, c) -> TreeAbove (FileOrDir b) -> (Path Rel Dir, c)
      goAbove (d, fod) ta = case treeAboveNode ta of
        FodFile _ _ -> goMAbove (d, fod) (treeAboveAbove ta) -- Should never happen. Valid DirForestCursors disallow  this
        FodDir rp -> goMAbove (rp </> d, fod) (treeAboveAbove ta)
   in goMAbove ([reldir|./|], treeCurrent tc) (treeAbove tc)

dirForestCursorSelectPrevOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectPrevOnSameLevel f g = dirForestCursorForestCursorL $ forestCursorSelectPrevOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectNextOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectNextOnSameLevel f g = dirForestCursorForestCursorL $ forestCursorSelectNextOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectFirstOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectFirstOnSameLevel f g = dirForestCursorForestCursorL %~ forestCursorSelectFirstOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectLastOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectLastOnSameLevel f g = dirForestCursorForestCursorL %~ forestCursorSelectLastOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectPrevTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectPrevTree f g = dirForestCursorForestCursorL $ forestCursorSelectPrevTreeCursor (fmap f) (fmap g)

dirForestCursorSelectNextTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectNextTree f g = dirForestCursorForestCursorL $ forestCursorSelectNextTreeCursor (fmap f) (fmap g)

dirForestCursorSelectFirstTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectFirstTree f g = dirForestCursorForestCursorL %~ forestCursorSelectFirstTreeCursor (fmap f) (fmap g)

dirForestCursorSelectLastTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectLastTree f g = dirForestCursorForestCursorL %~ forestCursorSelectLastTreeCursor (fmap f) (fmap g)

dirForestCursorSelectPrev :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectPrev f g = dirForestCursorForestCursorL $ forestCursorSelectPrev (fmap f) (fmap g)

dirForestCursorSelectNext :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectNext f g = dirForestCursorForestCursorL $ forestCursorSelectNext (fmap f) (fmap g)

dirForestCursorSelectFirst :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectFirst f g = dirForestCursorForestCursorL %~ forestCursorSelectFirst (fmap f) (fmap g)

dirForestCursorSelectLast :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DirForestCursor a b
dirForestCursorSelectLast f g = dirForestCursorForestCursorL %~ forestCursorSelectLast (fmap f) (fmap g)

dirForestCursorSelectFirstChild :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectFirstChild f g = dirForestCursorForestCursorL $ forestCursorSelectBelowAtStart (fmap f) (fmap g)

dirForestCursorSelectLastChild :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectLastChild f g = dirForestCursorForestCursorL $ forestCursorSelectBelowAtEnd (fmap f) (fmap g)

dirForestCursorSelectParent :: (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectParent f g = dirForestCursorForestCursorL $ forestCursorSelectAbove (fmap f) (fmap g)

dirForestCursorDeleteCurrent :: (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorDeleteCurrent g = dirForestCursorForestCursorL $ forestCursorDeleteSubTree (fmap g)

dirForestCursorOpen :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorOpen = dirForestCursorForestCursorL forestCursorOpenCurrentForest

dirForestCursorClose :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorClose = dirForestCursorForestCursorL forestCursorCloseCurrentForest

dirForestCursorToggle :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorToggle = dirForestCursorForestCursorL forestCursorToggleCurrentForest

dirForestCursorOpenRecursively :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorOpenRecursively = dirForestCursorForestCursorL forestCursorOpenCurrentForestRecursively

dirForestCursorToggleRecursively :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorToggleRecursively = dirForestCursorForestCursorL forestCursorToggleCurrentForestRecursively
