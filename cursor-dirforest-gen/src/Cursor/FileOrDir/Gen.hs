{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.FileOrDir.Gen where

import Cursor.FileOrDir
import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.DirForest ()
import Test.QuickCheck

instance (GenValid a) => GenValid (FileOrDirCursor a) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

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
