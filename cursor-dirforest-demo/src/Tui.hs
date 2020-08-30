{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Cursor.Brick.Text
import Cursor.DirForest.Brick
import Cursor.FileOrDir
import Cursor.Simple.DirForest
import Cursor.Types
import qualified Data.DirForest as DF
import Data.Int
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro
import Path
import Path.IO
import System.Posix
import Text.Show.Pretty

cursorDirForestDemo :: IO ()
cursorDirForestDemo = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  pPrint endState

newtype TuiState
  = TuiState {stateCursor :: Maybe (DirForestCursor Int64)}
  deriving (Show, Eq)

data ResourceName
  = TheTextCursor
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

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let dfc = stateCursor ts
   in [maybe emptyWidget drawDirForestInt64Cursor dfc]

drawDirForestInt64Cursor :: DirForestCursor Int64 -> Widget ResourceName
drawDirForestInt64Cursor =
  verticalPaddedDirForestCursorWidget
    (withDefAttr selectedAttr . goFodC)
    goFod
    2
  where
    goFodC :: FileOrDirCursor Int64 -> Widget ResourceName
    goFodC = \case
      Existent fod -> goFod fod
      InProgress tc -> selectedTextCursorWidget TheTextCursor tc
    goFod :: FileOrDir Int64 -> Widget ResourceName
    goFod = \case
      FodFile rf i -> withDefAttr fileAttr $ str $ fromRelFile rf <> " " <> show i <> " bytes"
      FodDir rd -> withDefAttr dirAttr $ str $ fromRelDir rd

handleTuiEvent :: forall n e. TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let doP :: (DirForestCursor Int64 -> DeleteOrUpdate (DirForestCursor Int64)) -> EventM n (Next TuiState)
          doP func =
            continue $
              s
                { stateCursor = case stateCursor s of
                    Nothing -> Nothing
                    Just dfc -> case func dfc of
                      Deleted -> Nothing
                      Updated dfc' -> Just dfc'
                }
          doM :: (DirForestCursor Int64 -> DeleteOrUpdate (Maybe (DirForestCursor Int64))) -> EventM n (Next TuiState)
          doM func = doP $ \c -> case func c of
            Deleted -> Updated c
            Updated Nothing -> Updated c
            Updated (Just c') -> Updated c'
          doD :: (DirForestCursor Int64 -> Maybe (DeleteOrUpdate (DirForestCursor Int64))) -> EventM n (Next TuiState)
          doD func = doP $ \c -> case func c of
            Nothing -> Updated c
            Just Deleted -> Deleted
            Just (Updated c') -> Updated c'
          doMM :: (DirForestCursor Int64 -> Maybe (DirForestCursor Int64)) -> EventM n (Next TuiState)
          doMM func = doP $ \c -> Updated $ fromMaybe c $ func c
       in case stateCursor s of
            Nothing -> case vtye of
              EvKey (KChar 'n') [] -> doMM dirForestCursorStartNew
              _ -> continue s
            Just dfc -> case dfc ^. dirForestCursorSelectedL of
              InProgress _ ->
                case vtye of
                  EvKey KEsc [] -> doP dirForestCursorDeleteCurrent
                  EvKey (KChar c) [] -> doMM $ dirForestCursorInsertChar c
                  EvKey KBS [] -> doD dirForestCursorRemoveChar
                  EvKey KDel [] -> doD dirForestCursorDeleteChar
                  EvKey KLeft [] -> doMM dirForestCursorSelectPrevChar
                  EvKey KRight [] -> doMM dirForestCursorSelectNextChar
                  EvKey KEnter [] -> doMM $ dirForestCursorCompleteToFile 0
                  EvKey KEnter [MMeta] -> doMM dirForestCursorCompleteToDir
                  _ -> continue s
              Existent _ ->
                case vtye of
                  EvKey (KChar 'q') [] -> halt s
                  EvKey (KChar 'f') [] -> doM dirForestCursorSelectFirstChild
                  EvKey (KChar 'l') [] -> doM dirForestCursorSelectLastChild
                  EvKey (KChar 'j') [] -> doM dirForestCursorSelectNext
                  EvKey (KChar 'k') [] -> doM dirForestCursorSelectPrev
                  EvKey (KChar 'g') [] -> doP dirForestCursorSelectFirst
                  EvKey (KChar 'G') [] -> doP dirForestCursorSelectLast
                  EvKey (KChar 'p') [] -> doM dirForestCursorSelectParent
                  EvKey (KChar 'n') [] -> doMM dirForestCursorStartNew
                  EvKey (KChar 'd') [] -> doP dirForestCursorDeleteCurrent
                  EvKey KLeft [] -> doM dirForestCursorSelectParent
                  EvKey KRight [] -> doM dirForestCursorSelectLastChild
                  EvKey KDown [] -> doM dirForestCursorSelectNext
                  EvKey KUp [] -> doM dirForestCursorSelectPrev
                  EvKey (KChar '\t') [] -> doMM dirForestCursorToggle
                  EvKey KEnter [] -> doMM dirForestCursorToggle
                  _ -> continue s
    _ -> continue s
