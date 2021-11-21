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
import Cursor.Types
import qualified Data.DirForest as DF
import Data.Word
import Path
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxShrinks (const 0) $ do
  -- No shrinking until I figure out what the problem is
  genValidSpec @(DirForestCursor Word8)
  describe "shrinkValid DirForestCursor" $ do
    it "does not shrink the singletonFile dirforest cursor to itself" $
      let df = makeDirForestCursor $ DF.singletonFile [relfile|a|] 'a'
       in shrinkValid df `shouldNotSatisfy` elem df
    it "does not shrink the singletonDir dirforest cursor to itself" $
      let df = makeDirForestCursor $ DF.singletonDir [reldir|a|] :: Maybe (DirForestCursor Char)
       in shrinkValid df `shouldNotSatisfy` elem df
  -- it "does not shrink a value to itself" $ do
  --   shrinkValidDoesNotShrinkToItselfWithLimit @(DirForestCursor Word8) 1
  -- shrinkValidSpecWithLimit @(DirForestCursor Word8) 1
  -- xdescribe "Does not hold because of extra validity constraints" $ lensSpecOnValid (dirForestCursorMapCursorL @Word8)
  describe "makeDirForestCursor" $ do
    it "works for an empty dirforest" $
      shouldBeValid $
        makeDirForestCursor (DF.empty @Word8)
    it "produces valid cursors" $ producesValid (makeDirForestCursor @Word8)
  describe "dirForestCursorPrepareForMovement" $ it "produces valid results" $ producesValid (dirForestCursorPrepareForMovement @Word8)
  describe "rebuildDirForestCursor" $ do
    it "produces valid dirforests" $ producesValid (rebuildDirForestCursor @Word8)
    it "is the inverse of 'makeDirForestCursor'" $
      forAllValid $ \df ->
        case makeDirForestCursor @Word8 df of
          Nothing -> pure ()
          Just dfc -> case rebuildDirForestCursor dfc of
            Updated df' -> df' `shouldBe` df
            Deleted -> expectationFailure "Should rountrip"
  describe "dirForestCursorSelectPrevTree" $ forestMovementMSpec dirForestCursorSelectPrevTree
  describe "dirForestCursorSelectNextTree" $ forestMovementMSpec dirForestCursorSelectNextTree
  xdescribe "is not true because of subselections" $
    describe "dirForestCursorSelectPrevTree and dirForestCursorSelectNextTree" $
      inverseMMovementsSpec dirForestCursorSelectPrevTree dirForestCursorSelectNextTree
  describe "dirForestCursorSelectFirstTree" $ forestMovementSpec dirForestCursorSelectFirstTree
  describe "dirForestCursorSelectLastTree" $ forestMovementSpec dirForestCursorSelectLastTree
  xdescribe "is not true because of subselections" $
    describe "dirForestCursorSelectFirstTree and dirForestCursorSelectLastTree" $
      inverseMovementsSpec dirForestCursorSelectFirstTree dirForestCursorSelectLastTree
  describe "dirForestCursorSelectPrevOnSameLevel" $ forestMovementMSpec dirForestCursorSelectPrevOnSameLevel
  describe "dirForestCursorSelectNextOnSameLevel" $ forestMovementMSpec dirForestCursorSelectNextOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $
    describe "dirForestCursorSelectPrevOnSameLevel and dirForestCursorSelectNextOnSameLevel" $
      inverseMMovementsSpec dirForestCursorSelectPrevOnSameLevel dirForestCursorSelectNextOnSameLevel
  describe "dirForestCursorSelectFirstOnSameLevel" $ forestMovementSpec dirForestCursorSelectFirstOnSameLevel
  describe "dirForestCursorSelectLastOnSameLevel" $ forestMovementSpec dirForestCursorSelectLastOnSameLevel
  xdescribe "is not true because of ordering of files in the map" $
    describe "dirForestCursorSelectFirstOnSameLevel and dirForestCursorSelectLastOnSameLevel" $
      inverseMovementsSpec dirForestCursorSelectFirstOnSameLevel dirForestCursorSelectLastOnSameLevel
  describe "dirForestCursorSelectPrev" $ forestMovementMSpec dirForestCursorSelectPrev
  describe "dirForestCursorSelectNext" $ forestMovementMSpec dirForestCursorSelectNext
  describe "dirForestCursorSelectFirst" $ forestMovementSpec dirForestCursorSelectFirst
  describe "dirForestCursorSelectLast" $ forestMovementSpec dirForestCursorSelectLast
  describe "dirForestCursorSelectFirstChild" $ forestMovementMSpec dirForestCursorSelectFirstChild
  describe "dirForestCursorSelectLastChild" $ forestMovementMSpec dirForestCursorSelectLastChild
  describe "dirForestCursorSelectParent" $ do
    it "produces valid cursors" $ producesValid (dirForestCursorSelectParent @Word8)
    xdescribe "This does not hold, because in-progress file or directories will be deleted before moving" $ do
      it "is the inverse of dirForestCursorSelectFirstChild" $
        inverseMProp dirForestCursorSelectFirstChild dirForestCursorSelectParent
      it "is the inverse of dirForestCursorSelectLastChild" $
        inverseMProp dirForestCursorSelectLastChild dirForestCursorSelectParent
  describe "dirForestCursorDeleteCurrent" $ it "produces valid results" $ producesValid (dirForestCursorDeleteCurrent @Word8)
  describe "dirForestCursorStartNew" $ it "produces valid results" $ producesValid (dirForestCursorStartNew @Word8)
  describe "dirForestCursorStartNewBelowAtStart" $ it "produces valid results" $ producesValid (dirForestCursorStartNewBelowAtStart @Word8)
  describe "dirForestCursorStartNewBelowAtEnd" $ it "produces valid results" $ producesValid (dirForestCursorStartNewBelowAtEnd @Word8)
  describe "dirForestCursorStopNew" $ it "produces valid results" $ producesValid (dirForestCursorStopNew @Word8)
  describe "dirForestCursorInsertChar" $ it "produces valid results" $ producesValid2 (dirForestCursorInsertChar @Word8 @Word8)
  describe "dirForestCursorAppendChar" $ it "produces valid results" $ producesValid2 (dirForestCursorAppendChar @Word8 @Word8)
  describe "dirForestCursorRemoveChar" $ it "produces valid results" $ producesValid (dirForestCursorRemoveChar @Word8)
  describe "dirForestCursorDeleteChar" $ it "produces valid results" $ producesValid (dirForestCursorDeleteChar @Word8)
  describe "dirForestCursorSelectPrevChar" $ it "produces valid results" $ producesValid (dirForestCursorSelectPrevChar @Word8 @Word8)
  describe "dirForestCursorSelectNextChar" $ it "produces valid results" $ producesValid (dirForestCursorSelectNextChar @Word8 @Word8)
  describe "dirForestCursorCompleteToDir" $ it "produces valid results" $ producesValid (dirForestCursorCompleteToDir @Word8 @Word8)
  describe "dirForestCursorCompleteToFile" $ it "produces valid results" $ producesValid2 (dirForestCursorCompleteToFile @Word8 @Word8)

inverseMovementsSpec ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)) ->
  Spec
inverseMovementsSpec f1 f2 = do
  it "are inverses starting with the First" $
    inverseProp f1 f2
  it "are inverses starting with the Second" $
    inverseProp f2 f1

inverseProp ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)) ->
  Property
inverseProp f g = forAllValid $ \dfc -> case f @Word8 dfc of
  Deleted -> pure () -- Fine
  Updated dfc' -> case g @Word8 dfc' of
    Deleted -> pure () -- Fine
    Updated dfc'' -> dfc'' `shouldBe` dfc

inverseMMovementsSpec ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))) ->
  Spec
inverseMMovementsSpec f1 f2 = do
  it "are inverses starting with the First" $
    inverseMProp f1 f2
  it "are inverses starting with the Second" $
    inverseMProp f2 f1

inverseMProp ::
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))) ->
  (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))) ->
  Property
inverseMProp f g = forAllValid $ \dfc -> case f @Word8 dfc of
  Deleted -> pure () -- Fine
  Updated Nothing -> pure () -- Fine
  Updated (Just dfc') -> case g @Word8 dfc' of
    Deleted -> pure () -- Fine
    Updated Nothing -> expectationFailure "The second movement must not be a failure."
    Updated (Just dfc'') -> dfc'' `shouldBe` dfc

forestMovementMSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (Maybe (DirForestCursor a))) -> Spec
forestMovementMSpec func = do
  it "produces valid results" $ producesValid (func @Word8)
  it "is a movement" $
    forAllValid $ \dfc ->
      case func @Word8 dfc of
        Deleted -> pure () -- Fine
        Updated Nothing -> pure () -- Fine
        Updated (Just dfc') -> rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc

forestMovementSpec :: (forall a. (Show a, Eq a, GenValid a) => DirForestCursor a -> DeleteOrUpdate (DirForestCursor a)) -> Spec
forestMovementSpec func = do
  it "produces valid results" $ producesValid (func @Word8)
  it "is a movement" $
    forAllValid $ \dfc ->
      case func @Word8 dfc of
        Deleted -> pure () -- Fine
        Updated dfc' -> rebuildDirForestCursor dfc' `shouldBe` rebuildDirForestCursor dfc
