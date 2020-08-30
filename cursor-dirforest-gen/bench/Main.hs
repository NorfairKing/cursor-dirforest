{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Cursor.DirForest
import Cursor.DirForest.Gen ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(FileOrDir Int),
      genValidBench @(FileOrDirCursor Int),
      genValidBench @(DirForestCursor Int Int)
    ]
