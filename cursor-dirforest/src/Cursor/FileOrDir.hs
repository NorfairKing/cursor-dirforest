{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Cursor.FileOrDir where

import Control.DeepSeq
import Cursor.Text
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path

data FileOrDirCursor a
  = Existent (FileOrDir a)
  | InProgress TextCursor
  deriving (Show, Eq, Generic, Functor)

data FileOrDir a = FodFile (Path Rel File) a | FodDir (Path Rel Dir)
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (FileOrDir a) where
  validate fod =
    mconcat
      [ genericValidate fod,
        declare "The path is toplevel" $ case fod of
          FodFile rf _ -> isTopLevel rf
          FodDir rd -> isTopLevel rd
      ]

instance NFData a => NFData (FileOrDir a)

isTopLevel :: Path Rel t -> Bool
isTopLevel p_ = parent p_ == [reldir|./|]
