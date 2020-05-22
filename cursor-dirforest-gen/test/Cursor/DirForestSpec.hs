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

spec :: Spec
spec = do
  genValidSpec @(DirForestCursor Int)
  genValidSpec @(DirTreeCursor Int)
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
