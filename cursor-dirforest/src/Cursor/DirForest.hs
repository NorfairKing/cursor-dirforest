{-# LANGUAGE DeriveGeneric #-}

module Cursor.DirForest where

import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.Validity
import GHC.Generics (Generic)

data DirForestCursor a = DirForestCursor
  deriving (Show, Eq, Generic)

instance Validity a => Validity (DirForestCursor a)

makeDirForestCursor :: DirForest a -> DirForestCursor a
makeDirForestCursor = undefined

rebuildDirForestCursor :: DirForestCursor a -> DirForest a
rebuildDirForestCursor = undefined
