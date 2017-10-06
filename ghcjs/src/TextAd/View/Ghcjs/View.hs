{-# LANGUAGE    QuasiQuotes                  #-}
module TextAd.View.Ghcjs.View
  ( runGhcjs
  ) where

import BasicPrelude              hiding (on)
import Control.Monad.Morph       (hoist, generalize)
import Control.Monad.Trans.State (get, modify, evalState, evalStateT, State, StateT)
import Data.Char                 (toUpper)
import Data.Foldable             (traverse_)
import Data.Text.Lazy            (unpack)
import Lens.Family               ((^.), (.~))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet               (shamlet)
import GHCJS.DOM.Types           (Document, Element)
import GHCJS.DOM                 (currentDocument)
import GHCJS.DOM.Document        (getBody, getElementById, createElement)
import GHCJS.DOM.Element         (getInnerHTML, setInnerHTML, click, getScrollHeight, setScrollTop)
import GHCJS.DOM.Node            (appendChild, removeChild, getNextSibling, getFirstChild, Node, IsNode)
import GHCJS.DOM.EventM          (on)
import qualified Data.Text       as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM

import TextAd.Interpreter.Action
import TextAd.Interpreter.StoryBuilder
import TextAd.Model.Core
import TextAd.Model.History      as H
import TextAd.Model.Util
import TextAd.View.Ghcjs.Util    (readState, writeState, addHead)


type SS a = StateT Story IO a

data View = View { doc          :: Document
                 , roomArea     :: Element
                 , progressArea :: Element
                 , textArea     :: Element
                 , buttonArea   :: Element
                 , scrollHeight :: Int
                 }

nl :: Text
nl = "<br/><br/>"

-- Data.Text (toTitle) not available in js version of text - compiles but throws js ReferenceError in runtime...
toTitle :: Text -> Text
toTitle = T.pack . toTitle' . T.unpack

toTitle' :: String -> String
toTitle' (x: xs) = (toUpper x) : xs
toTitle' []      = []

updateHistory :: View -> (H.History -> H.History) -> SS ()
updateHistory v f = do
  oldPath <- readState (doc v)
  case H.deserialse oldPath of
    Right h -> writeState (doc v) $ H.serialise $ f h
    Left h  -> liftIO $ putStrLn $ "Failed to parse history: " <> h

-- | Runs the app as an in-browser app
runGhcjs :: MonadIO m => StoryBuilder () -> m ()
runGhcjs storyDef = do

  let story = toStory storyDef

  liftIO $ do
    Just d    <- currentDocument
    Just body <- getBody d

    addHead d "<meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">"
    addHead d "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css\"       integrity=\"sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7\" crossorigin=\"anonymous\">"
    addHead d "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css\" integrity=\"sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r\" crossorigin=\"anonymous\">"
    addHead d $ "<title>" <> story ^. sTitle <> "</title>"

    Just dv <- createElement d $ Just ("div" :: Text)
    setInnerHTML dv . Just . unpack $ renderHtml [shamlet|$newline always
      <div class="container" role="main">
        <div class="page-header">
          <h1>#{story ^. sTitle}
        <div class="row">
          <div class="panel panel-default">
            <div class="panel-heading">
              <table style="width: 100%;">
                <tr>
                  <td #room class="panel-title">
                  <td #progress class="text-right">
            <div class="panel-body">
              <div #textArea style="width: 100%; height: 50vh; overflow: auto; font-size: 130%;">
        <div class="row">
          <div #buttonArea>
    |]
    _ <- appendChild body (Just dv)

    -- Just script <- createElement doc (Just "script")
    -- appendChild body (Just script)
    -- setOuterHTML script $ Just "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js\" integrity=\"sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS\" crossorigin=\"anonymous\">"

    Just r  <- getElementById d ("room"       :: Text)
    Just p  <- getElementById d ("progress"   :: Text)
    Just ta <- getElementById d ("textArea"   :: Text)
    Just ba <- getElementById d ("buttonArea" :: Text)
    let v = View d r p ta ba 0

    path <- readState d

    (flip evalStateT) story $ do
      res <- hoistG $ H.initStory path
      case res of
        Right (_, initTxt) -> do
          when (not $ null initTxt) $ do
            appendText v $ intercalate nl initTxt <> nl
        Left (h, err) -> do
          putStrLn $ "Couldn't replay state: " <> err
          -- replace history with amount successfully restored
          liftIO $ writeState (doc v) $ H.serialise h

      rId <- (\s' -> s' ^. sPlayer ^. pLocation) <$> get
      displayRoom v rId
      loop v

  return ()

removeFrom :: Text -> Text -> Text
removeFrom ref from =
  case T.splitOn ref from of
    x : _ -> x
    []    -> from

updateTextArea :: View -> (Text -> Text) -> IO ()
updateTextArea v f = do
  let ta = textArea v
  Just txt <- getInnerHTML ta
  setInnerHTML ta $ Just $ f txt

removeTextSuffix :: View -> IO ()
removeTextSuffix v =
  updateTextArea v $ removeFrom ("<br><br><i>") -- normalised version of nl

appendText :: View -> Text -> SS ()
appendText v newTxt = do
  liftIO $ removeTextSuffix v
  liftIO $ updateTextArea v (<> newTxt)
  addTextSuffix v

addTextSuffix :: View -> SS ()
addTextSuffix v = do
  story <- get
  room  <- hoistG $ toRoom $ story ^. sPlayer ^. pLocation

  -- list room items
  items <- hoistG $ listItems room
  let itemsText = if null $ room ^. rItems then "" else (nl <> "<i>") <> items <> "</i>"

  -- list room exits
  exits <- hoistG $ listExits room
  let exitsText =
        if null exits
          then nl <> "<i>There are no exits visible</i>"
          else nl <> "<i>The following exits are visible: " <> (intercalate ", " $ toHtml <$> exits) <> "</i>"
            where colour N = "rgb(130,0,186)"
                  colour W = "rgb(0,100,0)"
                  colour E = "rgb(0,0,255)"
                  colour S = "rgb(255,0,0)"
                  colour U = "rgb(255,0,255)"
                  toHtml (Exit label dirHint _) = "<a id=\"" <> label <> "\" href=\"#\" style=\"color: " <> (colour dirHint) <> ";\">" <> label <> "</a>"

  liftIO $ updateTextArea v $ (<> itemsText <> exitsText)

  let g (Exit exitLabel _ exitRId) = do
        Just exitLink <- getElementById (doc v) exitLabel
        _ <- on exitLink click $ do
          -- as is SS () - we must execute it ourselves in IO

          liftIO $ (flip evalStateT) story $ do
            goto' v exitLabel exitRId

        return ()
  _ <- liftIO $ sequence $ map g exits

  return ()

hoistG :: State Story a -> StateT Story IO a
hoistG = hoist generalize

toObject2 :: Story -> Oid -> Object
toObject2 story oid = evalState (toObject oid) story

toRoom2 :: Story -> Rid -> Room
toRoom2 story rid = evalState (toRoom rid) story

updateProgress :: View -> SS ()
updateProgress v = do
  let p = progressArea v
  story <- get
  let mMaxScore = story ^. sMaxScore
      score     = story ^. sScore
  case mMaxScore of
    Just maxScore -> liftIO $ setInnerHTML p $ Just (show (100 * score `div` maxScore) <> "% completed")
    -- Just maxScore -> liftIO $ setInnerHTML p $ Just (show score <> "/" <> show maxScore)
    Nothing       -> liftIO $ setInnerHTML p $ Just ("Score" <> show score)

loop :: View -> SS ()
loop v = do
  -- get current scroll height to scroll to
  liftIO $ setScrollTop (textArea v) (scrollHeight v)
  h <- liftIO $ getScrollHeight (textArea v)
  let v' = v { scrollHeight = h }

  story <- get
  room <- hoistG $ toRoom $ story ^. sPlayer ^. pLocation
  let roomItems = room ^. rItems
      itemsToPickUp = filter (_oCanPickUp . toObject2 story) roomItems
      toTalkTo = [(t, atn) | (t, Just atn) <- (((^. oTitle) &&& (^. oTalk)) . toObject2 story) <$> roomItems]

  updateProgress v'

  let onClickTalk t atn = do
        updateHistory v' $ H.addTalk t
        sayAction v' atn

  showButtons v' False $
       [("Show inventory", showInventory v')]
    <> (if null itemsToPickUp then [] else [("Take an item",   takeItemI v' itemsToPickUp)])
    <> (if null roomItems     then [] else [("Examine",        examineI v' roomItems)])
    <> (if null toTalkTo      then [] else (\(t, atn) -> ("Talk to " <> t, onClickTalk t atn)) <$> toTalkTo)


appendTexts :: View -> [Text] -> SS ()
appendTexts v =
  appendText v . intercalate nl

displayRoom :: View -> Rid -> SS ()
displayRoom v rid = do
  room <- hoistG $ toRoom rid
  liftIO $ setInnerHTML (roomArea v) $ Just $ room ^. rTitle
  txts <- hoistG $ runAction $ room ^. rDescr
  appendTexts v txts

showButton :: MonadIO m => View -> Story -> (Text, SS ()) -> m ()
showButton v s (l, atn) = do
  Just btn <- createElement (doc v) $ Just ("span" :: Text)
  _ <- appendChild (buttonArea v) (Just btn)
  setInnerHTML btn . Just . unpack $ renderHtml [shamlet|$newline always
    <button class="btn btn-lg btn-default" type="button">
      #{l}
    &nbsp;
  |]

  _ <- liftIO $ on btn click $ do
    liftIO $ evalStateT atn s
  return ()

showButtons :: View -> Bool -> [(Text, SS ())] -> SS ()
showButtons v showBack options = do
  story <- get
  liftIO $ do
    clearButtons (buttonArea v)
    _ <- sequence $ map (showButton v story) options
    when showBack $ do
      showButton v story ("Back", loop v)
    return ()

clearButtons :: MonadIO m => Element -> m ()
clearButtons ba = do
  let gen mc = do ms <- maybe (return Nothing) getNextSibling mc
                  return $ fmap (\c -> (c, ms)) mc
  children <- getFirstChild ba >>= SM.toList . SM.unfoldrM gen
  traverse_ (removeChild ba . Just) $ children

showInventory :: View -> SS ()
showInventory v = do
  story <- get
  let inventory = story ^. (sPlayer . pInventory)
      inventoryMenu oid = showButtons v True [
        ("Examine " <> (toObject2 story oid ^. oTitle), examineO v oid),
        ("Use with"                                   , useWith v oid)]
  showButtons v True $ (toTitle . (^. oTitle) . toObject2 story &&& inventoryMenu) <$> inventory

takeItemI :: View -> [Oid] -> SS ()
takeItemI v items = do
  story <- get
  showButtons v True $ (("Take " <>) . (^. oTitle) . toObject2 story &&& takeO v) <$> items

takeO :: View -> Oid -> SS ()
takeO v oid = do
  hoistG $ takeItemS oid
  o <- hoistG $ toObject oid
  appendText v $ nl <> "You take " <> the o <> "."
  updateHistory v $ H.addTake (o ^.oTitle)
  loop v

examineI :: View -> [Oid] -> SS ()
examineI v items = do
  story <- get
  showButtons v True $ (("Examine " <>) . (^. oTitle) . toObject2 story &&& examineO v) <$> items

examineO :: View -> Oid -> SS ()
examineO v oid = do
  o     <- hoistG $ toObject oid
  descr <- hoistG $ runAction $ o ^. oDescr
  appendText v $ nl <> "You examine " <> the o <> ". " <> intercalate nl descr
  updateHistory v $ H.addExamine (o ^.oTitle)
  loop v

clearText :: View -> SS ()
clearText v =
  liftIO $ setInnerHTML (textArea v) $ Just ("" :: Text)

goto' :: View -> Text -> Action (Maybe Rid) -> SS ()
goto' v exitLabel roomAction = do
  eRid <- hoistG $ goto roomAction
  case eRid of
    Left txts -> appendText v $ nl <> intercalate nl txts
    Right rid -> do clearText v
                    displayRoom v rid
  updateHistory v $ H.addGo exitLabel
  loop v

useWith :: View -> Oid -> SS ()
useWith v oid1 = do
  story <- get
  let player = story ^. sPlayer
      items = player ^. pInventory <> toRoom2 story (player ^. pLocation) ^. rItems
  showButtons v True $ (toTitle . (^. oTitle) . toObject2 story &&& useWithO v oid1) <$> items

useWithO :: View -> Oid -> Oid -> SS ()
useWithO v oid1 oid2 = do
  o1 <- hoistG $ toObject oid1
  o2 <- hoistG $ toObject oid2
  appendText v $ nl <> "You use " <> the o1 <> " with " <> the o2 <> ". "
  updateHistory v $ H.addUse (o1 ^.oTitle) (o2 ^.oTitle)
  txts <- hoistG $ use oid1 oid2
  if null txts
    then appendText v "Nothing happens."
    else appendTexts v txts
  loop v

sayAction :: View -> Action () -> SS ()
sayAction v action = do
  modify $ sSay .~ []
  txt <- hoistG $ runAction action
  when (not $ null txt) $ appendText v $ nl <> intercalate nl txt
  story <- get
  let nextSayOptions = story ^. sSay
  if null nextSayOptions
    then loop v
    else showSayOptions v nextSayOptions

showSayOptions :: View -> [(Text, Action ())] -> SS ()
showSayOptions v sayOptions = do
  let onClick s action = do
        updateHistory v $ H.addSay s
        sayAction v action
  showButtons v True $ (\(s, action) -> ("Say \"" <> s <> "\"", onClick s action)) <$> sayOptions
