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
import Data.Word
import Debug.Trace
import Path
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics
import Test.Validity.Shrinking

spec :: Spec
spec = modifyMaxShrinks (const 0) $ do
  genValidSpec @(DirTreeCursor Word8)
  -- shrinkValidSpecWithLimit @(DirTreeCursor Word8) 1
  -- No shrinking until I figure out what the problem is
  genValidSpec @(DirForestCursor Word8)
  describe "shrinkValid DirForestCursor" $ do
    it "does not shrink the singleton dirforest cursor to itself" $
      let df = DF.singleton [relfile|a|] 'a'
       in shrinkValid df `shouldNotSatisfy` (elem df)
  -- it "does not shrink a value to itself" $ do
  --   shrinkValidDoesNotShrinkToItselfWithLimit @(DirForestCursor Word8) 1
  -- shrinkValidSpecWithLimit @(DirForestCursor Word8) 1
  xdescribe "Does not hold because of extra validity constraints" $ lensSpecOnValid (dirForestCursorMapCursorL @Word8)
  describe "makeDirForestCursor" $ do
    it "works for an empty dirforest" $ do
      shouldBeValid $ makeDirForestCursor (DF.empty @Word8)
    it "produces valid cursors" $ producesValidsOnValids (makeDirForestCursor @Word8)
  describe "rebuildDirForestCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirForestCursor @Word8)
  describe "makeDirTreeCursor" $ do
    it "works for a dirtree with an empty dirforest below"
      $ shouldBeValid
      $ makeDirTreeCursor (NodeDir (DF.empty @Word8))
    it "produces valid cursors" $ producesValidsOnValids (makeDirTreeCursor @Word8)
  describe "rebuildDirTreeCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirTreeCursor @Word8)
  describe "dirForestCursorSelectPrevTreeOnSameLevel" $ forestMovementMSpec dirForestCursorSelectPrevTreeOnSameLevel
  describe "dirForestCursorSelectNextTreeOnSameLevel" $ forestMovementMSpec dirForestCursorSelectNextTreeOnSameLevel
  xdescribe "is not true because of subselections" $ describe "dirForestCursorSelectPrevTreeOnSameLevel and dirForestCursorSelectNextTreeOnSameLevel" $ do
    inverseMMovementsSpec dirForestCursorSelectPrevTreeOnSameLevel dirForestCursorSelectNextTreeOnSameLevel
  describe "dirForestCursorSelectFirstTreeOnSameLevel" $ forestMovementSpec dirForestCursorSelectFirstTreeOnSameLevel
  describe "dirForestCursorSelectLastTreeOnSameLevel" $ forestMovementSpec dirForestCursorSelectLastTreeOnSameLevel
  xdescribe "is not true because of subselections" $ describe "dirForestCursorSelectFirstTreeOnSameLevel and dirForestCursorSelectLastTreeOnSameLevel" $
    inverseMovementsSpec dirForestCursorSelectFirstTreeOnSameLevel dirForestCursorSelectLastTreeOnSameLevel
  describe "dirForestCursorSelectPrevOnSameLevel" $ forestMovementMSpec dirForestCursorSelectPrevOnSameLevel
  describe "dirForestCursorSelectNextOnSameLevel" $ forestMovementMSpec dirForestCursorSelectNextOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $ describe "dirForestCursorSelectPrevOnSameLevel and dirForestCursorSelectNextOnSameLevel" $ do
    inverseMMovementsSpec dirForestCursorSelectPrevOnSameLevel dirForestCursorSelectNextOnSameLevel
  describe "dirForestCursorSelectFirstOnSameLevel" $ forestMovementSpec dirForestCursorSelectFirstOnSameLevel
  describe "dirForestCursorSelectLastOnSameLevel" $ forestMovementSpec dirForestCursorSelectLastOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $ describe "dirForestCursorSelectFirstOnSameLevel and dirForestCursorSelectLastOnSameLevel" $ do
    inverseMovementsSpec dirForestCursorSelectFirstOnSameLevel dirForestCursorSelectLastOnSameLevel
  describe "dirForestCursorSelectFirstChild" $ forestMovementMSpec dirForestCursorSelectFirstChild
  describe "dirForestCursorSelectLastChild" $ forestMovementMSpec dirForestCursorSelectLastChild
  describe "dirTreeCursorSelectFirstChild" $ treeMovementMSpec dirTreeCursorSelectFirstChild
  describe "dirTreeCursorSelectLastChild" $ treeMovementMSpec dirTreeCursorSelectLastChild

inverseMovementsSpec ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) ->
  Spec
inverseMovementsSpec f1 f2 = do
  it "are inverses starting with the First" $
    inverseFunctionsOnValid (f1 @Word8) (f2 @Word8)
  it "are inverses starting with the Second" $
    inverseFunctionsOnValid (f1 @Word8) (f2 @Word8)

inverseMMovementsSpec ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> Maybe (DirForestCursor a)) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> Maybe (DirForestCursor a)) ->
  Spec
inverseMMovementsSpec f1 f2 = do
  it "are inverses starting with the First" $
    inverseFunctionsIfSucceedOnValid (f1 @Word8) (f2 @Word8)
  it "are inverses starting with the Second" $
    inverseFunctionsIfSucceedOnValid (f1 @Word8) (f2 @Word8)

forestMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> Maybe (DirForestCursor a)) -> Spec
forestMovementMSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Word8)
  it "is a movement" $ forAllValid $ \dfc ->
    case func @Word8 dfc of
      Nothing -> pure () -- Fine
      Just dfc' -> rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc

forestMovementSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) -> Spec
forestMovementSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Word8)
  it "is a movement" $ forAllValid $ \dfc ->
    let dfc' = func @Word8 dfc
     in rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc

treeMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirTreeCursor a -> Maybe (DirTreeCursor a)) -> Spec
treeMovementMSpec func = do
  it "produces valid results" $ producesValidsOnValids (func @Word8)
  it "is a movement" $ forAllValid $ \dfc ->
    case func @Word8 dfc of
      Nothing -> pure () -- Fine
      Just dfc' -> rebuildDirTreeCursor dfc' `shouldBe` rebuildDirTreeCursor dfc
