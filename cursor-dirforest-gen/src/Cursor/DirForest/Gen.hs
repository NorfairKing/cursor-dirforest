{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.DirForest.Gen where

import Cursor.DirForest
import Cursor.Forest
import Cursor.Forest.Gen ()
import Cursor.List.NonEmpty
import Cursor.Tree
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.DirForest ()
import Data.Tree
import Test.QuickCheck

instance (GenValid a) => GenValid (FileOrDir a) where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ sized $ \s -> do
          (a, b) <- genSplit s
          rf <- resize a $ genValid `suchThat` isTopLevel
          v <- resize b genValid
          pure $ FodFile rf v,
        FodDir <$> (genValid `suchThat` isTopLevel)
      ]

instance (GenValid a, GenValid b) => GenValid (DirForestCursor a b) where
  shrinkValid = shrinkValidStructurally
  genValid = DirForestCursor <$> (ForestCursor <$> (NonEmptyCursor <$> genListOf goCTree <*> goTreeCursor <*> genListOf goCTree))
    where
      goForest :: Gen (Forest (FileOrDir b))
      goForest = genListOf goTree
      goTree :: Gen (Tree (FileOrDir b))
      goTree = do
        fod <- genValid
        f <- case fod of
          FodFile _ _ -> pure []
          FodDir _ -> goForest
        pure $ Node fod f
      goCForest :: Gen (CForest (FileOrDir b))
      goCForest =
        oneof
          [ pure EmptyCForest,
            ClosedForest <$> genNonEmptyOf goTree,
            OpenForest <$> genNonEmptyOf goCTree
          ]
      goCTree :: Gen (CTree (FileOrDir b))
      goCTree = do
        fod <- genValid
        cf <- case fod of
          FodFile _ _ -> pure EmptyCForest
          FodDir _ -> goCForest
        pure $ CNode fod cf
      goTreeCursor :: Gen (TreeCursor (FileOrDir a) (FileOrDir b))
      goTreeCursor = sized $ \s -> do
        (a, b, c) <- genSplit3 s
        treeAbove <- resize a goMAbove
        treeCurrent <- resize b genValid
        treeBelow <- resize c $ case treeCurrent of
          FodFile _ _ -> pure EmptyCForest
          FodDir _ -> goCForest
        pure TreeCursor {..}
      goMAbove :: Gen (Maybe (TreeAbove (FileOrDir b)))
      goMAbove = oneof [pure Nothing, Just <$> goAbove]
      goAbove :: Gen (TreeAbove (FileOrDir b))
      goAbove = sized $ \s -> do
        (a, b, c, d) <- genSplit4 s
        treeAboveLefts <- resize a $ genListOf goCTree
        treeAboveAbove <- resize b goMAbove
        treeAboveNode <- resize c $ FodDir <$> (genValid `suchThat` isTopLevel)
        treeAboveRights <- resize d $ genListOf goCTree
        pure TreeAbove {..}
