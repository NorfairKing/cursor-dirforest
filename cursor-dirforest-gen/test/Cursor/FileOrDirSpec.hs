{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.FileOrDirSpec
  ( spec,
  )
where

import Cursor.FileOrDir
import Cursor.FileOrDir.Gen ()
import Cursor.Simple.DirForest
import qualified Data.DirForest as DF
import Data.Word
import Path
import Test.Hspec
import Test.Hspec.QuickCheck
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
  describe "fileOrDirCursorInsert" $ it "produces valid results" $ producesValidsOnValids2 (fileOrDirCursorInsert @Word8)
  describe "fileOrDirCursorAppend" $ it "produces valid results" $ producesValidsOnValids2 (fileOrDirCursorAppend @Word8)
  describe "completeTextCursorToFile" $ it "produces valid results" $ producesValidsOnValids completeTextCursorToFile
  describe "completeTextCursorToDir" $ it "produces valid results" $ producesValidsOnValids completeTextCursorToDir
