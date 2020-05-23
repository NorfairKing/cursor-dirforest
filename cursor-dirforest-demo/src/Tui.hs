{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Cursor.DirForest.Brick
import Cursor.Simple.DirForest
import qualified Data.DirForest as DF
import Data.Int
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
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
      appAttrMap =
        const $
          attrMap
            defAttr
            [ (selectedAttr, bg white),
              (fileAttr, fg blue),
              (dirAttr, fg red)
            ]
    }

selectedAttr :: AttrName
selectedAttr = "selected"

fileAttr :: AttrName
fileAttr = "file"

dirAttr :: AttrName
dirAttr = "dir"

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
   in [maybe emptyWidget drawDirForestInt64Cursor dfc]

drawDirForestInt64Cursor :: DirForestCursor Int64 -> Widget n
drawDirForestInt64Cursor =
  verticalPaddedDirForestCursorWidget
    (withDefAttr selectedAttr . goFod)
    goFod
    2
  where
    goFod :: FileOrDir Int64 -> Widget n
    goFod = \case
      FodFile rf i -> withDefAttr fileAttr $ str $ fromRelFile rf <> " " <> show i <> " bytes"
      FodDir rd -> withDefAttr dirAttr $ str $ fromRelDir rd

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
            EvKey (KChar 'j') [] -> doM dirForestCursorSelectNext
            EvKey (KChar 'k') [] -> doM dirForestCursorSelectPrev
            EvKey (KChar 'g') [] -> doP dirForestCursorSelectFirst
            EvKey (KChar 'G') [] -> doP dirForestCursorSelectLast
            EvKey (KChar 'p') [] -> doM dirForestCursorSelectParent
            EvKey KLeft [] -> doM dirForestCursorSelectParent
            EvKey KRight [] -> doM dirForestCursorSelectLastChild
            EvKey KDown [] -> doM dirForestCursorSelectNext
            EvKey KUp [] -> doM dirForestCursorSelectPrev
            EvKey (KChar '\t') [] -> doM dirForestCursorToggle
            _ -> continue s
    _ -> continue s
