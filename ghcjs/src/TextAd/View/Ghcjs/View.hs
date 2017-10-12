{-# LANGUAGE    QuasiQuotes                  #-}
module TextAd.View.Ghcjs.View
  ( runGhcjs
  ) where

import BasicPrelude              hiding (on)
import Control.Monad.Morph       (hoist, generalize)
import Control.Monad.Trans.State (State, StateT, get, modify, evalState, evalStateT)
import Data.Char                 (toUpper)
import Data.Either               (isLeft)
import Data.Foldable             (traverse_)
import Data.Text.Lazy            (unpack)
import Lens.Family               ((^.), (.~))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet               (shamlet)
import GHCJS.DOM.Types           (Document, Element)
import GHCJS.DOM                 (currentDocument)
import GHCJS.DOM.Document        (getBody, getElementById, createElement)
import GHCJS.DOM.Element         (getInnerHTML, setInnerHTML, click, getScrollHeight, setScrollTop)
import GHCJS.DOM.Node            (appendChild, removeChild, getNextSibling, getFirstChild)
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
  where
    toTitle' :: String -> String
    toTitle' (x: xs) = (toUpper x) : xs
    toTitle' []      = []

updateHistory :: View -> (H.History -> H.History) -> SS ()
updateHistory v f = do
  oldPath <- readState (doc v)
  case H.deserialse oldPath of
    Right h -> writeState (doc v) $ H.serialise $ f h
    Left h  -> liftIO $ putStrLn $ "Failed to parse history: " <> h

hoistG :: State Story a -> StateT Story IO a
hoistG = hoist generalize

toObject2 :: Story -> Oid -> Object
toObject2 story oid = evalState (toObject oid) story

toRoom2 :: Story -> Rid -> Room
toRoom2 story rid = evalState (toRoom rid) story

-- | Runs the app as an in-browser app
runGhcjs :: MonadIO m => StoryBuilder () -> m ()
runGhcjs storyDef = do

  let story = toStory storyDef

  Just d    <- liftIO $ currentDocument
  Just body <- liftIO $ getBody d

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

  liftIO $ (flip evalStateT) story $ do
    res <- hoistG $ H.initStory path
    case res of
      Right (_, initTxt) -> do
        when (not $ null initTxt) $ do
          appendText v $ intercalate nl initTxt <> nl
      Left (h, err) -> do
        putStrLn $ "Couldn't replay state: " <> err
        -- replace history with amount successfully restored
        writeState (doc v) $ H.serialise h

    rId <- (\s' -> s' ^. sPlayer ^. pLocation) <$> get
    displayRoom v rId
    loop v

--  return ()

removeFrom :: Text -> Text -> Text
removeFrom ref from =
  case T.splitOn ref from of
    x : _ -> x
    []    -> from

updateTextArea :: MonadIO m => View -> (Text -> Text) -> m ()
updateTextArea v f = do
  let ta = textArea v
  Just txt <- getInnerHTML ta
  setInnerHTML ta $ Just $ f txt

removeTextSuffix :: MonadIO m => View -> m ()
removeTextSuffix v =
  updateTextArea v $ removeFrom ("<br><br><i>") -- normalised version of nl

appendText :: View -> Text -> SS ()
appendText v newTxt = do
  removeTextSuffix v
  updateTextArea v (<> newTxt)
  addTextSuffix v

appendTexts :: View -> [Text] -> SS ()
appendTexts v =
  appendText v . intercalate nl

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

  updateTextArea v $ (<> itemsText <> exitsText)

  liftIO $ (flip traverse_) exits $ \(Exit exitLabel _ exitRId) -> do
    Just exitLink <- getElementById (doc v) exitLabel
    on exitLink click $ do
      -- as is SS () - we must execute it ourselves in IO
      liftIO $ (flip evalStateT) story $ do
        gotoS v exitLabel exitRId

updateProgress :: View -> SS ()
updateProgress v = do
  let p = progressArea v
  story <- get
  let mMaxScore = story ^. sMaxScore
      score     = story ^. sScore
  case mMaxScore of
    Just maxScore -> setInnerHTML p $ Just (show (100 * score `div` maxScore) <> "% completed")
    -- Just maxScore -> setInnerHTML p $ Just (show score <> "/" <> show maxScore)
    Nothing       -> setInnerHTML p $ Just ("Score" <> show score)

accessibleItems :: SS [Oid]
accessibleItems = do
  story <- get
  let player =  story ^. sPlayer
      room   =  toRoom2 story (player ^. pLocation)
      items  =  player ^. pInventory
             <> room   ^. rItems
  return items

scroll :: MonadIO m => View -> m View
scroll v = do
  -- get current scroll height to scroll to
  setScrollTop (textArea v) (scrollHeight v)
  h <- getScrollHeight (textArea v)
  return $ v { scrollHeight = h }

loop :: View -> SS ()
loop v = do

  story <- get
  room <- hoistG $ toRoom $ story ^. sPlayer ^. pLocation
  let roomItems     =  room ^. rItems
      itemsToPickUp =  filter (_oCanPickUp . toObject2 story) roomItems
      inventory     =  story ^. (sPlayer . pInventory)
      itemsToUse    =  {-inventory
                    <>-} filter (isLeft . _oUse . toObject2 story) roomItems
      toTalkTo      =  [(t, atn) | (t, Just atn) <- (((^. oTitle) &&& (^. oTalk)) . toObject2 story) <$> roomItems]

  v' <- scroll v

  updateProgress v'

  showButtons v' False $
       (if null inventory     then [] else [("Show inventory", onClickInventory v' inventory    )])
    <> (if null itemsToPickUp then [] else [("Take an item"  , onClickTake      v' itemsToPickUp)])
    <> (if null itemsToUse    then [] else [("Use"           , onClickUse       v' itemsToUse   )])
    <> (if null roomItems     then [] else [("Examine"       , onClickExamine   v' roomItems    )])
    <> (if null toTalkTo      then [] else (\(t, atn) -> ("Talk to " <> t, onClickTalk v' t atn)) <$> toTalkTo)

displayRoom :: View -> Rid -> SS ()
displayRoom v rid = do
  room <- hoistG $ toRoom rid
  setInnerHTML (roomArea v) $ Just $ room ^. rTitle
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

clearText :: MonadIO m => View -> m ()
clearText v =
  setInnerHTML (textArea v) $ Just ("" :: Text)

gotoS :: View -> Text -> Action (Maybe Rid) -> SS ()
gotoS v exitLabel roomAction = do
  eRid <- hoistG $ goto roomAction
  case eRid of
    Left txts -> appendText v $ nl <> intercalate nl txts
    Right rid -> do clearText v
                    displayRoom v rid
  updateHistory v $ H.addGo exitLabel
  loop v

onClickInventory :: View -> [Oid] -> SS ()
onClickInventory v inventory = do
  story <- get
  showButtons v True $ (toTitle . (^. oTitle) . toObject2 story &&& onClickInventoryO v) <$> inventory

onClickInventoryO :: View -> Oid -> SS ()
onClickInventoryO v oid = do
  story <- get
  let title = toObject2 story oid ^. oTitle
      suffix = case toObject2 story oid ^. oUse of
                 Left  _ -> ""
                 Right _ -> " with "
  showButtons v True
     [ ("Examine " <> title          , onClickExamineO v oid)
     , ("Use "     <> title <> suffix, onClickUseO     v oid)
     ]

onClickTake :: View -> [Oid] -> SS ()
onClickTake v items = do
  story <- get
  showButtons v True $ (("Take " <>) . (^. oTitle) . toObject2 story &&& onClickTakeO v) <$> items

onClickTakeO :: View -> Oid -> SS ()
onClickTakeO v oid = do
  hoistG $ takeItemS oid
  o <- hoistG $ toObject oid
  appendText v $ nl <> "You take " <> the o <> "."
  updateHistory v $ H.addTake (o ^.oTitle)
  loop v

onClickExamine :: View -> [Oid] -> SS ()
onClickExamine v items = do
  story <- get
  showButtons v True $ (("Examine " <>) . (^. oTitle) . toObject2 story &&& onClickExamineO v) <$> items

onClickExamineO :: View -> Oid -> SS ()
onClickExamineO v oid = do
  o     <- hoistG $ toObject oid
  descr <- hoistG $ runAction $ o ^. oDescr
  appendText v $ nl <> "You examine " <> the o <> ". " <> intercalate nl descr
  updateHistory v $ H.addExamine (o ^.oTitle)
  loop v

onClickUse :: View -> [Oid] -> SS ()
onClickUse v items = do
  story <- get
  showButtons v True $ ((^. oTitle) . toObject2 story &&& onClickUseO v) <$> items

onClickUseO :: View -> Oid -> SS ()
onClickUseO v oid = do
  o <- hoistG $ toObject oid
  case o ^. oUse of
    Left  _ -> onClickUseItselfO v oid
    Right _ -> do
               items <- accessibleItems
               onClickUseWith v oid $ filter (/= oid) items

onClickUseWith :: View -> Oid -> [Oid] -> SS ()
onClickUseWith v oid1 items = do
  story <- get
  showButtons v True $ (toTitle . (^. oTitle) . toObject2 story &&& onClickUseWithO v oid1) <$> items

onClickUseWithO :: View -> Oid -> Oid -> SS ()
onClickUseWithO v oid1 oid2 = do
  o1 <- hoistG $ toObject oid1
  o2 <- hoistG $ toObject oid2
  appendText v $ nl <> "You use " <> the o1 <> " with " <> the o2 <> ". "
  updateHistory v $ H.addUse (o1 ^.oTitle) (Just $ o2 ^.oTitle)
  txts <- hoistG $ use oid1 (Just oid2)
  if null txts
    then appendText v "Nothing happens."
    else appendTexts v txts
  loop v

onClickUseItselfO :: View -> Oid -> SS ()
onClickUseItselfO v oid = do
  o <- hoistG $ toObject oid
  appendText v $ nl <> "You use " <> the o <> ". "
  updateHistory v $ H.addUse (o ^.oTitle) Nothing
  txts <- hoistG $ use oid Nothing
  if null txts
    then appendText v "Nothing happens."
    else appendTexts v txts
  loop v

onClickTalk :: View -> Text -> Action () -> SS ()
onClickTalk v t atn = do
  updateHistory v $ H.addTalk t
  sayAction v atn

onClickSay :: View -> Text -> Action () -> SS ()
onClickSay v s atn = do
  updateHistory v $ H.addSay s
  sayAction v atn

sayAction :: View -> Action () -> SS ()
sayAction v atn = do
    modify $ sSay .~ []
    txt <- hoistG $ runAction atn
    when (not $ null txt) $ appendText v $ nl <> intercalate nl txt
    story <- get
    let nextSayOptions = story ^. sSay
    if null nextSayOptions
      then loop v
      else showSayOptions nextSayOptions
  where
    showSayOptions :: [(Text, Action ())] -> SS ()
    showSayOptions sayOptions =
      showButtons v True $ (\(s, atn') -> ("Say \"" <> s <> "\"", onClickSay v s atn')) <$> sayOptions
