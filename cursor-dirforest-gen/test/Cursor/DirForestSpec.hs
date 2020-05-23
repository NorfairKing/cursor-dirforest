{-# LANGUAGE QuasiQuotes #-}
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
import Debug.Trace
import Path
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics
import Test.Validity.Shrinking

spec :: Spec
spec = modifyMaxShrinks (const 0) $ do
  genValidSpec @(DirTreeCursor Int)
  -- shrinkValidSpecWithLimit @(DirTreeCursor Int) 1
  -- No shrinking until I figure out what the problem is
  genValidSpec @(DirForestCursor Int)
  describe "shrinkValid DirForestCursor" $ do
    it "does not shrink the singleton dirforest cursor to itself" $
      let df = DF.singleton [relfile|a|] 'a'
       in shrinkValid df `shouldNotSatisfy` (elem df)
  -- it "does not shrink a value to itself" $ do
  --   shrinkValidDoesNotShrinkToItselfWithLimit @(DirForestCursor Int) 1
  -- shrinkValidSpecWithLimit @(DirForestCursor Int) 1
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
  describe "dirForestCursorSelectPrevOnSameLevel and dirForestCursorSelectNextOnSameLevel" $ do
    it "are inverses starting with Prev" $
      inverseFunctionsIfSucceedOnValid (dirForestCursorSelectPrevOnSameLevel @Int . traceShowId) (dirForestCursorSelectNextOnSameLevel . traceShowId)
    it "are inverses starting with Next" $
      inverseFunctionsIfSucceedOnValid (dirForestCursorSelectNextOnSameLevel @Int) dirForestCursorSelectPrevOnSameLevel
  describe "dirForestCursorSelectFirstOnSameLevel" $ forestMovementSpec dirForestCursorSelectFirstOnSameLevel
  describe "dirForestCursorSelectLastOnSameLevel" $ forestMovementSpec dirForestCursorSelectLastOnSameLevel
  describe "dirForestCursorSelectFirstOnSameLevel and dirForestCursorSelectLastOnSameLevel" $ do
    it "are inverses starting with First" $
      inverseFunctionsOnValid (dirForestCursorSelectFirstOnSameLevel @Int . traceShowId) (dirForestCursorSelectLastOnSameLevel . traceShowId)
    it "are inverses starting with Last" $
      inverseFunctionsOnValid (dirForestCursorSelectLastOnSameLevel @Int) dirForestCursorSelectFirstOnSameLevel
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

forestMovementSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) -> Spec
forestMovementSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Int)
  it "is a movement" $ forAllValid $ \dfc ->
    let dfc' = func @Int dfc
     in rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc

treeMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirTreeCursor a -> Maybe (DirTreeCursor a)) -> Spec
treeMovementMSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Int)
  it "is a movement" $ forAllValid $ \dfc ->
    case func @Int dfc of
      Nothing -> pure () -- Fine
      Just dfc' -> rebuildDirTreeCursor dfc' `shouldBe` rebuildDirTreeCursor dfc
