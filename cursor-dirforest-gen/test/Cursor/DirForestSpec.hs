{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.DirForestSpec
  ( spec,
  )
where

import Cursor.DirForest
import Cursor.DirForest.Gen ()
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @(DirForestCursor Int)
  genValidSpec @(DirTreeCursor Int)
  xdescribe "Does not hold because of extra validity constraints" $ lensSpecOnValid (dirForestCursorMapCursorL @Int)
  describe "makeDirForestCursor" $ do
    it "works for an empty dirforest" $ do
      shouldBeValid $ makeDirForestCursor (DF.empty @Int)
    it "produces valid cursors" $ producesValidsOnValids (makeDirForestCursor @Int)
  describe "rebuildDirForestCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirForestCursor @Int)
  describe "makeDirTreeCursor" $ do
    it "works for a dirtree with an empty dirforest below"
      $ shouldBeValid
      $ makeDirTreeCursor (NodeDir (DF.empty @Int))
    it "produces valid cursors" $ producesValidsOnValids (makeDirTreeCursor @Int)
  describe "rebuildDirTreeCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirTreeCursor @Int)
  describe "dirForestCursorSelectPrevOnSameLevel" $ forestMovementMSpec dirForestCursorSelectPrevOnSameLevel
  describe "dirForestCursorSelectNextOnSameLevel" $ forestMovementMSpec dirForestCursorSelectNextOnSameLevel
  describe "dirForestCursorSelectFirstChild" $ forestMovementMSpec dirForestCursorSelectFirstChild
  describe "dirForestCursorSelectLastChild" $ forestMovementMSpec dirForestCursorSelectLastChild
  describe "dirTreeCursorSelectFirstChild" $ treeMovementMSpec dirTreeCursorSelectFirstChild
  describe "dirTreeCursorSelectLastChild" $ treeMovementMSpec dirTreeCursorSelectLastChild

forestMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> Maybe (DirForestCursor a)) -> Spec
forestMovementMSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Int)
  it "is a movement" $ forAllValid $ \dfc ->
    case func @Int dfc of
      Nothing -> pure () -- Fine
      Just dfc' -> rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc

treeMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirTreeCursor a -> Maybe (DirTreeCursor a)) -> Spec
treeMovementMSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Int)
  it "is a movement" $ forAllValid $ \dfc ->
    case func @Int dfc of
      Nothing -> pure () -- Fine
      Just dfc' -> rebuildDirTreeCursor dfc' `shouldBe` rebuildDirTreeCursor dfc
