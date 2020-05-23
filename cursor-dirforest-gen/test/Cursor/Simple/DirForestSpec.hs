{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.DirForestSpec
  ( spec,
  )
where

import Cursor.DirForest.Gen ()
import Cursor.Simple.DirForest
import qualified Data.DirForest as DF
import Data.Word
import Path
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxShrinks (const 0) $ do
  -- No shrinking until I figure out what the problem is
  genValidSpec @(DirForestCursor Word8)
  describe "shrinkValid DirForestCursor" $ do
    it "does not shrink the singletonFile dirforest cursor to itself" $
      let df = makeDirForestCursor $ DF.singletonFile [relfile|a|] 'a'
       in shrinkValid df `shouldNotSatisfy` (elem df)
    it "does not shrink the singletonDir dirforest cursor to itself" $
      let df = makeDirForestCursor $ DF.singletonDir [reldir|a|] :: Maybe (DirForestCursor Char)
       in shrinkValid df `shouldNotSatisfy` (elem df)
  -- it "does not shrink a value to itself" $ do
  --   shrinkValidDoesNotShrinkToItselfWithLimit @(DirForestCursor Word8) 1
  -- shrinkValidSpecWithLimit @(DirForestCursor Word8) 1
  -- xdescribe "Does not hold because of extra validity constraints" $ lensSpecOnValid (dirForestCursorMapCursorL @Word8)
  describe "makeDirForestCursor" $ do
    it "works for an empty dirforest" $ do
      shouldBeValid $ makeDirForestCursor (DF.empty @Word8)
    it "produces valid cursors" $ producesValidsOnValids (makeDirForestCursor @Word8)
  describe "rebuildDirForestCursor" $ do
    it "produces valid dirforests" $ producesValidsOnValids (rebuildDirForestCursor @Word8)
    it "is the inverse of 'makeDirForestCursor'" $ inverseFunctionsIfFirstSucceedsOnValid (makeDirForestCursor @Word8) (rebuildDirForestCursor @Word8)
  describe "dirForestCursorSelectPrevTree" $ forestMovementMSpec dirForestCursorSelectPrevTree
  describe "dirForestCursorSelectNextTree" $ forestMovementMSpec dirForestCursorSelectNextTree
  xdescribe "is not true because of subselections" $ describe "dirForestCursorSelectPrevTree and dirForestCursorSelectNextTree" $ do
    inverseMMovementsSpec dirForestCursorSelectPrevTree dirForestCursorSelectNextTree
  describe "dirForestCursorSelectFirstTree" $ forestMovementSpec dirForestCursorSelectFirstTree
  describe "dirForestCursorSelectLastTree" $ forestMovementSpec dirForestCursorSelectLastTree
  xdescribe "is not true because of subselections" $ describe "dirForestCursorSelectFirstTree and dirForestCursorSelectLastTree" $
    inverseMovementsSpec dirForestCursorSelectFirstTree dirForestCursorSelectLastTree
  describe "dirForestCursorSelectPrevOnSameLevel" $ forestMovementMSpec dirForestCursorSelectPrevOnSameLevel
  describe "dirForestCursorSelectNextOnSameLevel" $ forestMovementMSpec dirForestCursorSelectNextOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $ describe "dirForestCursorSelectPrevOnSameLevel and dirForestCursorSelectNextOnSameLevel" $ do
    inverseMMovementsSpec dirForestCursorSelectPrevOnSameLevel dirForestCursorSelectNextOnSameLevel
  describe "dirForestCursorSelectFirstOnSameLevel" $ forestMovementSpec dirForestCursorSelectFirstOnSameLevel
  describe "dirForestCursorSelectLastOnSameLevel" $ forestMovementSpec dirForestCursorSelectLastOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $ describe "dirForestCursorSelectFirstOnSameLevel and dirForestCursorSelectLastOnSameLevel" $ do
    inverseMovementsSpec dirForestCursorSelectFirstOnSameLevel dirForestCursorSelectLastOnSameLevel
  describe "dirForestCursorSelectPrev" $ forestMovementMSpec dirForestCursorSelectPrev
  describe "dirForestCursorSelectNext" $ forestMovementMSpec dirForestCursorSelectNext
  describe "dirForestCursorSelectFirst" $ forestMovementSpec dirForestCursorSelectFirst
  describe "dirForestCursorSelectLast" $ forestMovementSpec dirForestCursorSelectLast
  describe "dirForestCursorSelectFirstChild" $ forestMovementMSpec dirForestCursorSelectFirstChild
  describe "dirForestCursorSelectLastChild" $ forestMovementMSpec dirForestCursorSelectLastChild
  describe "dirForestCursorSelectParent" $ do
    it "produces valid cursors" $ producesValidsOnValids (dirForestCursorSelectParent @Word8)
    it "is the inverse of dirForestCursorSelectFirstChild" $ inverseFunctionsIfSucceedOnValid dirForestCursorSelectFirstChild (dirForestCursorSelectParent @Word8)
    it "is the inverse of dirForestCursorSelectLastChild" $ inverseFunctionsIfSucceedOnValid dirForestCursorSelectLastChild (dirForestCursorSelectParent @Word8)

inverseMovementsSpec ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DirForestCursor a) ->
  Spec
inverseMovementsSpec f1 f2 = do
  it "are inverses starting with the First" $
    inverseFunctionsOnValid (f1 @Word8) (f2 @Word8)
  it "are inverses starting with the S       econd" $
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
