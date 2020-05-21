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
  describe "makeDirForestCursor" $ it "produces valid cursors" $ producesValidsOnValids (makeDirForestCursor @Int)
  describe "rebuildDirForestCursor" $ it "produces valid dirforests" $ producesValidsOnValids (rebuildDirForestCursor @Int)
