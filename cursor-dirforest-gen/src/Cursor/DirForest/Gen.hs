{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.DirForest.Gen where

import Cursor.DirForest
import Cursor.DirForest
import Cursor.Map.Gen
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.DirForest
import Debug.Trace
import Path
import qualified System.FilePath as FP
import Test.QuickCheck

instance (Show a, GenValid a, Ord a) => GenValid (DirForestCursor a) where
  shrinkValid = traceShowId . shrinkValidStructurally
  genValid = DirForestCursor <$> genMapCursorByDependent go1 go2 go3
    where
      isTopLevel p_ = parent p_ == [reldir|./|]
      go3 :: Gen (FilePath, DirTree a)
      go3 = go1
      go1 :: Gen (FilePath, DirTree a)
      go1 = sized $ \s -> do
        (a, b) <- genSplit s
        dt <- resize a genValid
        fp <- resize b $ case dt of
          NodeFile _ -> fromRelFile <$> (genValid `suchThat` isTopLevel)
          NodeDir _ -> (FP.dropTrailingPathSeparator . fromRelDir) <$> (genValid `suchThat` isTopLevel)
        pure (fp, dt)
      go2 :: Gen (FilePath, Maybe (DirForestCursor a))
      go2 = sized $ \s -> do
        (a, b) <- genSplit s
        mdfc <- resize a genValid
        fp <- resize b $ FP.dropTrailingPathSeparator . fromRelDir <$> (genValid `suchThat` isTopLevel)
        pure (fp, mdfc)

instance (Show a, GenValid a, Ord a) => GenValid (DirTreeCursor a) where
  shrinkValid = shrinkValidStructurally
  genValid = sized $ \n ->
    scale (\x -> max 0 $ x - 1) $
      oneof
        [ DirTreeCursorFile <$> genValid,
          DirTreeCursorDir <$> genValid
        ]
