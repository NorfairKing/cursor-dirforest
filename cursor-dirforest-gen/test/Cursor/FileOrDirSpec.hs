{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.FileOrDirSpec
  ( spec,
  )
where

import Cursor.FileOrDir
import Cursor.FileOrDir.Gen ()
import Data.Word
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(FileOrDirCursor Word8)
  genValidSpec @(FileOrDir Word8)
  describe "makeFileOrDirCursor" $ it "produces valid cursors" $ producesValid (makeFileOrDirCursor @Word8)
  describe "rebuildFileOrDirCursor" $ do
    it "produces valid cursors" $
      producesValid
        (rebuildFileOrDirCursor @Word8)
    it "roundtrips with makeFileOrDirCursor" $ forAllValid $ \fod -> rebuildFileOrDirCursor (makeFileOrDirCursor @Word8 fod) `shouldBe` Just fod
  describe "fileOrDirCursorInsertChar" $ it "produces valid results" $ producesValid2 (fileOrDirCursorInsertChar @Word8)
  describe "fileOrDirCursorAppendChar" $ it "produces valid results" $ producesValid2 (fileOrDirCursorAppendChar @Word8)
  describe "fileOrDirCursorRemoveChar" $ it "produces valid results" $ producesValid (fileOrDirCursorRemoveChar @Word8)
  describe "fileOrDirCursorDeleteChar" $ it "produces valid results" $ producesValid (fileOrDirCursorDeleteChar @Word8)
  describe "fileOrDirCursorSelectPrevChar" $ it "produces valid results" $ producesValid (fileOrDirCursorSelectPrevChar @Word8)
  describe "fileOrDirCursorSelectNextChar" $ it "produces valid results" $ producesValid (fileOrDirCursorSelectNextChar @Word8)
  describe "fileOrDirCursorCompleteToDir" $ it "produces valid results" $ producesValid (fileOrDirCursorCompleteToDir @Word8)
  describe "completeTextCursorToFile" $ it "produces valid results" $ producesValid completeTextCursorToFile
  describe "completeTextCursorToDir" $ it "produces valid results" $ producesValid completeTextCursorToDir
