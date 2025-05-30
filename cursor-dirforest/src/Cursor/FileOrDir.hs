{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cursor.FileOrDir where

import Control.DeepSeq
import Cursor.Text
import Cursor.Types
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path

data FileOrDirCursor a
  = Existent (FileOrDir a)
  | InProgress TextCursor
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (FileOrDirCursor a)

instance (NFData a) => NFData (FileOrDirCursor a)

makeFileOrDirCursor :: FileOrDir a -> FileOrDirCursor a
makeFileOrDirCursor = Existent

rebuildFileOrDirCursor :: FileOrDirCursor a -> Maybe (FileOrDir a)
rebuildFileOrDirCursor = \case
  Existent fod -> Just fod
  InProgress _ -> Nothing

fileOrDirCursorInsertChar :: Char -> FileOrDirCursor a -> Maybe (FileOrDirCursor a)
fileOrDirCursorInsertChar c = \case
  Existent _ -> Nothing
  InProgress tc -> InProgress <$> textCursorInsert c tc

fileOrDirCursorAppendChar :: Char -> FileOrDirCursor a -> Maybe (FileOrDirCursor a)
fileOrDirCursorAppendChar c = \case
  Existent _ -> Nothing
  InProgress tc -> InProgress <$> textCursorAppend c tc

fileOrDirCursorRemoveChar :: FileOrDirCursor a -> Maybe (DeleteOrUpdate (FileOrDirCursor a))
fileOrDirCursorRemoveChar = \case
  Existent _ -> Nothing
  InProgress tc -> fmap InProgress <$> textCursorRemove tc

fileOrDirCursorDeleteChar :: FileOrDirCursor a -> Maybe (DeleteOrUpdate (FileOrDirCursor a))
fileOrDirCursorDeleteChar = \case
  Existent _ -> Nothing
  InProgress tc -> fmap InProgress <$> textCursorDelete tc

fileOrDirCursorSelectPrevChar :: FileOrDirCursor a -> Maybe (FileOrDirCursor a)
fileOrDirCursorSelectPrevChar = \case
  Existent _ -> Nothing
  InProgress tc -> InProgress <$> textCursorSelectPrev tc

fileOrDirCursorSelectNextChar :: FileOrDirCursor a -> Maybe (FileOrDirCursor a)
fileOrDirCursorSelectNextChar = \case
  Existent _ -> Nothing
  InProgress tc -> InProgress <$> textCursorSelectNext tc

fileOrDirCursorCompleteToDir :: FileOrDirCursor a -> Maybe (Path Rel Dir, FileOrDirCursor a)
fileOrDirCursorCompleteToDir = fileOrDirCursorCompleteToDir' pure

fileOrDirCursorCompleteToDir' :: (Path Rel Dir -> Maybe (Path Rel Dir)) -> FileOrDirCursor a -> Maybe (Path Rel Dir, FileOrDirCursor a)
fileOrDirCursorCompleteToDir' func = \case
  Existent _ -> Nothing
  InProgress tc -> case completeTextCursorToDir tc of
    Nothing -> Nothing
    Just rd ->
      if isTopLevel rd
        then do
          rd' <- func rd
          if isTopLevel rd'
            then pure (rd', Existent $ FodDir rd')
            else Nothing
        else Nothing

completeTextCursorToDir :: TextCursor -> Maybe (Path Rel Dir)
completeTextCursorToDir = parseRelDir . T.unpack . rebuildTextCursor

fileOrDirCursorCompleteToFile :: a -> FileOrDirCursor a -> Maybe (Path Rel File, FileOrDirCursor a)
fileOrDirCursorCompleteToFile = fileOrDirCursorCompleteToFile' pure

fileOrDirCursorCompleteToFile' :: (Path Rel File -> Maybe (Path Rel File)) -> a -> FileOrDirCursor a -> Maybe (Path Rel File, FileOrDirCursor a)
fileOrDirCursorCompleteToFile' func a = \case
  Existent _ -> Nothing
  InProgress tc -> case completeTextCursorToFile tc of
    Nothing -> Nothing
    Just rf ->
      if isTopLevel rf
        then do
          rf' <- func rf
          if isTopLevel rf'
            then Just (rf', Existent $ FodFile rf' a)
            else Nothing
        else Nothing

completeTextCursorToFile :: TextCursor -> Maybe (Path Rel File)
completeTextCursorToFile = parseRelFile . T.unpack . rebuildTextCursor

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

instance (NFData a) => NFData (FileOrDir a)

isTopLevel :: Path Rel t -> Bool
isTopLevel p_ = parent p_ == [reldir|./|]
