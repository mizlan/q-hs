{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Edit qualified as E
import Control.Lens hiding (zoom)
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Graphics.Vty
import Network.HTTP.Req
import System.Exit
import System.Process (rawSystem)

data AppState = AppState
  { _queryEditor :: E.Editor Text (),
    _entries :: Vector Text,
    _entrySelectPos :: Maybe Int,
    _produce :: Text -> IO (Vector Text),
    _confirm :: Text -> IO ()
  }

makeLenses ''AppState

pattern Ctrl k = EvKey (KChar k) [MCtrl]

handleEvent :: BrickEvent () () -> EventM () AppState ()
handleEvent (VtyEvent (EvKey KEsc [])) = halt
handleEvent (VtyEvent (Ctrl k)) | k `elem` ['n', 'p'] = do
  st <- get
  let numEntries = length (st ^. entries)
  when (numEntries == 0) continueWithoutRedraw
  let n' = st ^. entrySelectPos <&> \n -> max (n - 1) 0
  put $ st & entrySelectPos .~ n'
handleEvent (VtyEvent (EvKey KEnter [])) = do
  st <- get
  let results = st ^. entries
      numEntries = length results
      query = head $ E.getEditContents (st ^. queryEditor)
  if T.null query
    then continueWithoutRedraw
    else do
      let qSearch = case st ^. entrySelectPos of
            Just i -> results ! i
            Nothing -> query
      liftIO $ (st ^. confirm) qSearch
      halt
handleEvent v = do
  st <- get
  let query = head $ E.getEditContents (st ^. queryEditor)
  let oldEntries = st ^. entries
  zoom queryEditor $ E.handleEditorEvent v
  st' <- get
  let query' = head $ E.getEditContents (st' ^. queryEditor)
  void $ runMaybeT do
    guard (query /= query')
    results <- liftIO $ (st ^. produce) query'
    guard (oldEntries /= results)
    put $ st' & entries .~ results & entrySelectPos .~ Nothing

buildSearch :: Text -> Text
buildSearch q = "https://www.google.com/search?q=" <> q <> " site:reddit.com"

searchResults :: Maybe Int -> Vector Text -> Widget ()
searchResults idx xs
  | V.null xs = txt "nothing to see here"
  | otherwise = case idx of
    Nothing -> V.foldr1 (<=>) . V.map txt $ xs
    Just i -> V.foldr1 (<=>) (V.map txt xs & ix i %~ withAttr (attrName "selected"))

dict :: IO (Vector Text)
dict = V.fromList <$> (take 800 <$> (T.lines <$> T.readFile "/usr/share/dict/words"))

dictSource :: Text -> IO (Vector Text)
dictSource q = do
  V.take 10 . V.filter (T.isPrefixOf q) <$> dict

googleSource :: Text -> IO (Vector Text)
googleSource "" = pure $ pure "nothing to see here"
googleSource q = do
  r <- runReq defaultHttpConfig $ do
    req
      GET
      (http "google.com" /: "complete" /: "search")
      NoReqBody
      jsonResponse
      (("client" =: ("firefox" :: Text)) <> ("q" =: q))
  case responseBody r of
    _ : t@(Array xs) : _ -> pure $ V.fromList [x | String x <- V.toList xs]
    _ -> pure V.empty

drawUI (AppState q e s _ _) = [E.renderEditor (txt . T.unlines) True q <=> searchResults s e]

attrs =
  [ (attrName "selected", fg blue),
    (E.editAttr, bg black)
  ]

openBrowser :: Text -> IO ()
openBrowser = void . rawSystem "open" . pure . T.unpack

app :: App AppState () ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap defAttr attrs
    }

setupApp :: (Text -> IO (Vector Text)) -> (Text -> IO ()) -> AppState
setupApp = AppState (E.editorText () (Just 1) "") V.empty Nothing

main :: IO ()
main = do
  void $ defaultMain app (setupApp googleSource (openBrowser . buildSearch))
