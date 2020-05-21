{-# LANGUAGE DeriveGeneric #-}

module Cursor.DirForest.Gen where

import Cursor.DirForest
import Data.DirForest (DirForest)
import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.DirForest

instance GenUnchecked a => GenUnchecked (DirForestCursor a)

instance (GenValid a, Ord a) => GenValid (DirForestCursor a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
