{-# LANGUAGE TypeApplications #-}

module Cursor.DirForestSpec
  ( spec,
  )
where

import Cursor.DirForest
import Cursor.DirForest.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(DirForestCursor Int)
  genValidSpec @(DirTreeCursor Int)
  describe "makeDirForestCursor" $ it "produces valid cursors" $ producesValidsOnValids (makeDirForestCursor @Int)
  describe "rebuildDirForestCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirForestCursor @Int)
  describe "makeDirTreeCursor" $ it "produces valid cursors" $ producesValidsOnValids (makeDirTreeCursor @Int)
  describe "rebuildDirTreeCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirTreeCursor @Int)
