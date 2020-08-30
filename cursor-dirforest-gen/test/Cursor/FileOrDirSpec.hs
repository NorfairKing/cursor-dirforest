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
  describe "makeFileOrDirCursor" $ it "produces valid cursors" $ producesValidsOnValids (makeFileOrDirCursor @Word8)
  describe "rebuildFileOrDirCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids
        (rebuildFileOrDirCursor @Word8)
    it "roundtrips with makeFileOrDirCursor" $ forAllValid $ \fod -> rebuildFileOrDirCursor (makeFileOrDirCursor @Word8 fod) `shouldBe` Just fod
  describe "fileOrDirCursorInsertChar" $ it "produces valid results" $ producesValidsOnValids2 (fileOrDirCursorInsertChar @Word8)
  describe "fileOrDirCursorAppendChar" $ it "produces valid results" $ producesValidsOnValids2 (fileOrDirCursorAppendChar @Word8)
  describe "fileOrDirCursorRemoveChar" $ it "produces valid results" $ producesValidsOnValids (fileOrDirCursorRemoveChar @Word8)
  describe "fileOrDirCursorDeleteChar" $ it "produces valid results" $ producesValidsOnValids (fileOrDirCursorDeleteChar @Word8)
  describe "fileOrDirCursorSelectPrevChar" $ it "produces valid results" $ producesValidsOnValids (fileOrDirCursorSelectPrevChar @Word8)
  describe "fileOrDirCursorSelectNextChar" $ it "produces valid results" $ producesValidsOnValids (fileOrDirCursorSelectNextChar @Word8)
  describe "fileOrDirCursorCompleteToDir" $ it "produces valid results" $ producesValidsOnValids (fileOrDirCursorCompleteToDir @Word8)
  describe "completeTextCursorToFile" $ it "produces valid results" $ producesValidsOnValids completeTextCursorToFile
  describe "completeTextCursorToDir" $ it "produces valid results" $ producesValidsOnValids completeTextCursorToDir
