{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.DirForest
import Cursor.DirForest.Brick
import Cursor.Map
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..), DirTree (..))
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.Directory
import System.Posix
import Text.Show.Pretty

cursorDirForestDemo :: IO ()
cursorDirForestDemo = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  pPrint endState

data TuiState
  = TuiState {stateCursor :: Maybe (DirForestCursor Int64)}
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap defAttr [(selectedAttr, fg white)]
    }

selectedAttr :: AttrName
selectedAttr = "selected"

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDir
  df <- DF.readNonHidden here $ \fp -> do
    COff size <- fileSize <$> getFileStatus (fromAbsFile fp)
    pure size
  pure $ TuiState $ makeDirForestCursor df

drawTui :: TuiState -> [Widget n]
drawTui ts =
  let dfc = stateCursor ts
   in [vBox [maybe emptyWidget drawDirForestInt64Cursor dfc, str (ppShow dfc)]]

drawDirForestInt64Cursor :: DirForestCursor Int64 -> Widget n
drawDirForestInt64Cursor =
  verticalDirForestCursorWidget
    goDirTreePair
    goKVC
    goDirTreePair
  where
    goDirTreePair :: FilePath -> DirTree Int64 -> Widget n
    goDirTreePair fp = \case
      NodeFile a -> str fp <+> str " " <+> goInt64 a
      NodeDir df -> str fp <=> padLeft (Pad 2) (drawDirForest df)
    drawDirForest :: DirForest Int64 -> Widget n
    drawDirForest (DirForest m) = vBox $ map (uncurry goDirTreePair) $ M.toList m
    goDirTreeCursorPair :: FilePath -> DirTreeCursor Int64 -> Widget n
    goDirTreeCursorPair fp =
      withAttr selectedAttr
        . dirTreeCursorWidget
          (\a -> str fp <+> str " " <+> goInt64 a)
          ( \mdf -> case mdf of
              Nothing -> str fp
              Just df -> str fp <=> drawDirForestInt64Cursor df
          )
    goKeySelected fp = \case
      NodeFile a -> withAttr selectedAttr (str fp) <+> str " " <+> goInt64 a
      NodeDir df -> withAttr selectedAttr (str fp) <=> padLeft (Pad 2) (drawDirForest df)
    goValueSelected fp mdfc = str fp <=> maybe emptyWidget (padLeft (Pad 2) . drawDirForestInt64Cursor) mdfc
    goKVC = keyValueWidget goKeySelected goValueSelected
    goInt64 i = str (show i) <+> str " bytes"

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let doP func =
            continue $
              s
                { stateCursor = case stateCursor s of
                    Nothing -> Nothing
                    Just dfc -> Just $ func dfc
                }
          doM func = doP (\c -> fromMaybe c $ func c)
       in case vtye of
            EvKey (KChar 'q') [] -> halt s
            EvKey (KChar 'f') [] -> doM dirForestCursorSelectFirstChild
            EvKey (KChar 'l') [] -> doM dirForestCursorSelectLastChild
            EvKey (KChar 'j') [] -> doM dirForestCursorSelectNextOnSameLevel
            EvKey (KChar 'k') [] -> doM dirForestCursorSelectPrevOnSameLevel
            EvKey KDown [] -> doM dirForestCursorSelectNextOnSameLevel
            EvKey KUp [] -> doM dirForestCursorSelectPrevOnSameLevel
            _ -> continue s
    _ -> continue s
