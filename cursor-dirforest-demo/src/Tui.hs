{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.DirForest
import Cursor.Map
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..), DirTree (..))
import Data.Int
import qualified Data.Map as M
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.Directory
import System.Posix

cursorDirForestDemo :: IO ()
cursorDirForestDemo = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

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
      appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDir
  df <- DF.read here $ \fp -> do
    COff size <- fileSize <$> getFileStatus (fromAbsFile fp)
    pure size
  pure $ TuiState $ makeDirForestCursor df

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [maybe emptyWidget (drawDirForestCursor $ \i -> str (show i) <+> str " bytes") (stateCursor ts)]

drawDirForestCursor :: forall a n. (a -> Widget n) -> DirForestCursor a -> Widget n
drawDirForestCursor func = verticalMapCursorWidget (drawDirTreePair func) goKVC (drawDirTreePair func) . dirForestCursorMapCursor
  where
    goKVC :: KeyValueCursor FilePath (DirTreeCursor a) FilePath (DirTree a) -> Widget n
    goKVC = keyValueWidget (drawDirTreePair func) (drawDirTreeCursorPair func)

drawDirTreePair :: (a -> Widget n) -> FilePath -> DirTree a -> Widget n
drawDirTreePair func fp dt = vBox [str fp, padLeft (Pad 2) $ drawDirTree func dt]

drawDirTreeCursorPair :: (a -> Widget n) -> FilePath -> DirTreeCursor a -> Widget n
drawDirTreeCursorPair func fp dtc = vBox [str fp, padLeft (Pad 2) $ drawDirTreeCursor func dtc]

drawDirTree :: (a -> Widget n) -> DirTree a -> Widget n
drawDirTree func = \case
  NodeFile a -> func a
  NodeDir df -> drawDirForest func df

drawDirForest :: (a -> Widget n) -> DirForest a -> Widget n
drawDirForest func (DirForest m) = vBox $ map (uncurry $ drawDirTreePair func) $ M.toList m

drawDirTreeCursor :: (a -> Widget n) -> DirTreeCursor a -> Widget n
drawDirTreeCursor func = \case
  DirTreeCursorFile a -> func a
  DirTreeCursorDir mdfc -> maybe emptyWidget (drawDirForestCursor func) mdfc

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s
