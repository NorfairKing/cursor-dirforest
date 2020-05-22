{-# LANGUAGE DeriveGeneric #-}

module Cursor.DirForest.Gen where

import Cursor.DirForest
import Cursor.Map.Gen
import Data.DirForest (DirForest)
import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.DirForest

instance (GenValid a, Ord a) => GenValid (DirForestCursor a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenValid a, Ord a) => GenValid (DirTreeCursor a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
