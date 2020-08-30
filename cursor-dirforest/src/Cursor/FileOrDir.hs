{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cursor.FileOrDir where

import Control.DeepSeq
import Cursor.Text
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path

data FileOrDirCursor a
  = Existent (FileOrDir a)
  | InProgress TextCursor
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (FileOrDirCursor a)

instance NFData a => NFData (FileOrDirCursor a)

makeFileOrDirCursor :: FileOrDir a -> FileOrDirCursor a
makeFileOrDirCursor = Existent

rebuildFileOrDirCursor :: FileOrDirCursor a -> Maybe (FileOrDir a)
rebuildFileOrDirCursor = \case
  Existent fod -> Just fod
  InProgress _ -> Nothing

completeTextCursorToFile :: TextCursor -> Maybe (Path Rel File)
completeTextCursorToFile = parseRelFile . T.unpack . rebuildTextCursor

completeTextCursorToDir :: TextCursor -> Maybe (Path Rel Dir)
completeTextCursorToDir = parseRelDir . T.unpack . rebuildTextCursor

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
