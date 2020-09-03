{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.DirForest
  ( -- * Types
    DirForestCursor (..),
    FileOrDirCursor (..),
    FileOrDir (..),

    -- * Construction and deconstruction

    -- ** Make
    makeDirForestCursor,

    -- ** Rebuild
    rebuildDirForestCursor,
    isTopLevel,

    -- *** Helper
    dirForestCursorPrepareForMovement,

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
    dirForestCursorSelectPrevChar,
    dirForestCursorSelectNextChar,

    -- * Edits
    dirForestCursorDeleteCurrent,
    dirForestCursorStartNew,
    dirForestCursorStartNewBelowAtStart,
    dirForestCursorStartNewBelowAtEnd,
    dirForestCursorInsertChar,
    dirForestCursorAppendChar,
    dirForestCursorRemoveChar,
    dirForestCursorDeleteChar,
    dirForestCursorCompleteToDir,
    dirForestCursorCompleteToFile,

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
import Cursor.Text
import Cursor.Tree
import Cursor.Types
import Data.DirForest (DirForest (..), DirTree (..))
import Data.Functor.Identity
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
newtype DirForestCursor a b = DirForestCursor {dirForestCursorForestCursor :: ForestCursor (FileOrDirCursor a) (FileOrDir b)}
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
                                      InProgress _ -> True
                                      Existent (FodFile _ _) -> case treeBelow tc of
                                        EmptyCForest -> True
                                        _ -> False
                                      Existent (FodDir _) -> True,
                                  decorate "The treeBelow" $ goCForest $ treeBelow tc
                                ],
                        decorate "The next trees" $ mconcat $ map goCTree $ nonEmptyCursorNext nec
                        -- TODO the names need to be unique
                      ]
      ]

instance (NFData a, NFData b) => NFData (DirForestCursor a b)

dirForestCursorForestCursorL :: Lens' (DirForestCursor a b) (ForestCursor (FileOrDirCursor a) (FileOrDir b))
dirForestCursorForestCursorL = lens dirForestCursorForestCursor $ \dfc mc -> dfc {dirForestCursorForestCursor = mc}

-- | The selected 'FileOrDir'.
--
-- Note that its path will only be the last piece, not the entire path.
-- If you need the entire piece, see 'dirForestCursorSelected' instead.
dirForestCursorSelectedL :: Lens' (DirForestCursor a b) (FileOrDirCursor a)
dirForestCursorSelectedL = dirForestCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL

-- | Make a 'DirForestCursor'.
--
-- This will fail if the dirforest is empty.
makeDirForestCursor :: (b -> a) -> DirForest b -> Maybe (DirForestCursor a b)
makeDirForestCursor func = fmap (DirForestCursor . makeForestCursor (fmap func . makeFileOrDirCursor) . NE.map makeCTree) . NE.nonEmpty . toForest
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

dirForestCursorPrepareForMovement :: (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (ForestCursor (FileOrDir a) (FileOrDir b))
dirForestCursorPrepareForMovement g dfc =
  fmap (mapForestCursor (fromJust . rebuildFileOrDirCursor) id)
    $ ( case dfc ^. dirForestCursorSelectedL of
          InProgress _ -> forestCursorRemoveElem (fmap g . makeFileOrDirCursor)
          Existent _ -> Updated
      )
    $ dirForestCursorForestCursor dfc

rebuildDirForestCursor :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForest b)
rebuildDirForestCursor f g = fmap (fromForest . NE.toList . NE.map rebuildCTree . rebuildForestCursor (fmap f)) . dirForestCursorPrepareForMovement g
  where
    fromForest :: Forest (FileOrDir b) -> DirForest b
    fromForest = goF
      where
        goF :: Forest (FileOrDir b) -> DirForest b
        goF = DirForest . M.fromList . map goT
        goT :: Tree (FileOrDir b) -> (FilePath, DirTree b)
        goT (Node fod forest) = case fod of
          FodFile rf v -> (fromRelFile rf, NodeFile v)
          FodDir rd -> (FP.dropTrailingPathSeparator $ fromRelDir rd, NodeDir $ goF forest)

foldDirForestCursor :: ([CTree (FileOrDir b)] -> TreeCursor (FileOrDirCursor a) (FileOrDir b) -> [CTree (FileOrDir b)] -> c) -> DirForestCursor a b -> c
foldDirForestCursor func (DirForestCursor fc) = foldForestCursor func fc

dirForestCursorSelected :: DirForestCursor a b -> (Path Rel Dir, FileOrDirCursor a)
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

doMovementF :: forall a b f. Functor f => (b -> a) -> (ForestCursor (FileOrDir a) (FileOrDir b) -> f (ForestCursor (FileOrDir a) (FileOrDir b))) -> DirForestCursor a b -> DeleteOrUpdate (f (DirForestCursor a b))
doMovementF g movementFunc =
  fmap
    ( fmap (DirForestCursor . mapForestCursor makeFileOrDirCursor id)
        . movementFunc
    )
    . dirForestCursorPrepareForMovement g

doMovement :: forall a b. (b -> a) -> (ForestCursor (FileOrDir a) (FileOrDir b) -> ForestCursor (FileOrDir a) (FileOrDir b)) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
doMovement g func = fmap runIdentity . doMovementF g (Identity . func)

dirForestCursorSelectPrevOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectPrevOnSameLevel f g = doMovementF g (forestCursorSelectPrevOnSameLevel (fmap f) (fmap g))

dirForestCursorSelectNextOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectNextOnSameLevel f g = doMovementF g $ forestCursorSelectNextOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectFirstOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectFirstOnSameLevel f g = doMovement g $ forestCursorSelectFirstOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectLastOnSameLevel :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectLastOnSameLevel f g = doMovement g $ forestCursorSelectLastOnSameLevel (fmap f) (fmap g)

dirForestCursorSelectPrevTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectPrevTree f g = doMovementF g $ forestCursorSelectPrevTreeCursor (fmap f) (fmap g)

dirForestCursorSelectNextTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectNextTree f g = doMovementF g $ forestCursorSelectNextTreeCursor (fmap f) (fmap g)

dirForestCursorSelectFirstTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectFirstTree f g = doMovement g $ forestCursorSelectFirstTreeCursor (fmap f) (fmap g)

dirForestCursorSelectLastTree :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectLastTree f g = doMovement g $ forestCursorSelectLastTreeCursor (fmap f) (fmap g)

dirForestCursorSelectPrev :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectPrev f g = doMovementF g $ forestCursorSelectPrev (fmap f) (fmap g)

dirForestCursorSelectNext :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectNext f g = doMovementF g $ forestCursorSelectNext (fmap f) (fmap g)

dirForestCursorSelectFirst :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectFirst f g = doMovement g $ forestCursorSelectFirst (fmap f) (fmap g)

dirForestCursorSelectLast :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorSelectLast f g = doMovement g $ forestCursorSelectLast (fmap f) (fmap g)

dirForestCursorSelectFirstChild :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectFirstChild f g = doMovementF g $ forestCursorSelectBelowAtStart (fmap f) (fmap g)

dirForestCursorSelectLastChild :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectLastChild f g = doMovementF g $ forestCursorSelectBelowAtEnd (fmap f) (fmap g)

dirForestCursorSelectParent :: (a -> b) -> (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (Maybe (DirForestCursor a b))
dirForestCursorSelectParent f g = doMovementF g $ forestCursorSelectAbove (fmap f) (fmap g)

dirForestCursorSelectPrevChar :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectPrevChar = dirForestCursorSelectedL fileOrDirCursorSelectPrevChar

dirForestCursorSelectNextChar :: DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorSelectNextChar = dirForestCursorSelectedL fileOrDirCursorSelectNextChar

dirForestCursorDeleteCurrent :: (b -> a) -> DirForestCursor a b -> DeleteOrUpdate (DirForestCursor a b)
dirForestCursorDeleteCurrent g = dirForestCursorForestCursorL $ forestCursorDeleteSubTree (fmap g . makeFileOrDirCursor)

dirForestCursorStartNew :: forall a b. (a -> b) -> (b -> a) -> Maybe (DirForestCursor a b) -> Maybe (DirForestCursor a b)
dirForestCursorStartNew f g =
  let tc = singletonTreeCursor $ InProgress emptyTextCursor
      new = DirForestCursor $ ForestCursor $ singletonNonEmptyCursor tc
   in \case
        Nothing -> Just new
        Just dfc -> case dfc ^. dirForestCursorSelectedL of
          InProgress _ -> Nothing
          Existent _ ->
            Just $ case dirForestCursorPrepareForMovement g dfc of
              Deleted -> new
              Updated fc ->
                DirForestCursor
                  $ forestCursorInsertAndSelectTreeCursor (fmap f . fromJust . rebuildFileOrDirCursor) tc
                  $ mapForestCursor
                    Existent
                    id
                    fc

dirForestCursorStartNewBelowAtStart :: forall a b. (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorStartNewBelowAtStart f g dfc = case dfc ^. dirForestCursorSelectedL of
  InProgress _ -> Nothing
  Existent (FodFile _ _) -> Nothing
  Existent (FodDir _) -> case dirForestCursorPrepareForMovement g dfc of
    Deleted -> Nothing -- Should not happen
    Updated fc ->
      Just $ DirForestCursor $ forestCursorAddChildNodeSingleToNodeAtStartAndSelect (fmap f . fromJust . rebuildFileOrDirCursor) (InProgress emptyTextCursor) $
        mapForestCursor
          Existent
          id
          fc

dirForestCursorStartNewBelowAtEnd :: forall a b. (a -> b) -> (b -> a) -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorStartNewBelowAtEnd f g dfc = case dfc ^. dirForestCursorSelectedL of
  InProgress _ -> Nothing
  Existent (FodFile _ _) -> Nothing
  Existent (FodDir _) -> case dirForestCursorPrepareForMovement g dfc of
    Deleted -> Nothing -- Should not happen
    Updated fc ->
      Just $ DirForestCursor $ forestCursorAddChildNodeSingleToNodeAtEndAndSelect (fmap f . fromJust . rebuildFileOrDirCursor) (InProgress emptyTextCursor) $
        mapForestCursor
          Existent
          id
          fc

dirForestCursorInsertChar :: Char -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorInsertChar c = dirForestCursorSelectedL $ fileOrDirCursorInsertChar c

dirForestCursorAppendChar :: Char -> DirForestCursor a b -> Maybe (DirForestCursor a b)
dirForestCursorAppendChar c = dirForestCursorSelectedL $ fileOrDirCursorAppendChar c

dirForestCursorRemoveChar :: DirForestCursor a b -> Maybe (DeleteOrUpdate (DirForestCursor a b))
dirForestCursorRemoveChar = focusPossibleDeleteOrUpdate dirForestCursorSelectedL fileOrDirCursorRemoveChar

dirForestCursorDeleteChar :: DirForestCursor a b -> Maybe (DeleteOrUpdate (DirForestCursor a b))
dirForestCursorDeleteChar = focusPossibleDeleteOrUpdate dirForestCursorSelectedL fileOrDirCursorDeleteChar

dirForestCursorCompleteToDir :: DirForestCursor a b -> Maybe (Path Rel Dir, DirForestCursor a b)
dirForestCursorCompleteToDir dfc = do
  (rd, fodc) <- fileOrDirCursorCompleteToDir $ dfc ^. dirForestCursorSelectedL
  let dfc' = dfc & dirForestCursorSelectedL .~ fodc
  pure (rd, dfc')

dirForestCursorCompleteToFile :: a -> DirForestCursor a b -> Maybe (Path Rel File, DirForestCursor a b)
dirForestCursorCompleteToFile a dfc = do
  (rf, fodc) <- fileOrDirCursorCompleteToFile a $ dfc ^. dirForestCursorSelectedL
  let dfc' = dfc & dirForestCursorSelectedL .~ fodc
  pure (rf, dfc')

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
