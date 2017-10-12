module TextAd.Model.Util where

import BasicPrelude
import Control.Monad             (msum)
import Control.Monad.Trans.State (State, get, modify)
import Control.Error.Safe        (justErr, headErr)
import Lens.Family               ((%~), (.~), (^.))
import Lens.Family.Stock         (at, _Just)
import qualified Data.Text       as T
import qualified Data.Map        as M

import TextAd.Model.Core
import TextAd.Interpreter.Action
import TextAd.Interpreter.ExitsBuilder
import TextAd.Interpreter.UseAction

import Debug.Trace (trace)

toObject :: Oid -> State Story Object
toObject oid = do
  story <- get
  case M.lookup oid $ _sObjects story of
    Just o  -> return o
    Nothing -> error' $ "Object " <> show' oid <> " has not been added to story"

toRoom :: Rid -> State Story Room
toRoom rid = do
  story <- get
  case story ^. sRooms . at rid of
    Just r  -> return r
    Nothing -> error' $ "Room " <> show' rid <> " has not been added to story"

a :: Object -> Text
a obj = pronoun <> " " <> obj ^. oTitle
  where pronoun = case obj ^. oNounType of
          Proper     -> ""
          Quantitive -> "some"
          Particular -> if startsWithVowel $ T.unpack $ obj ^. oTitle then "an" else "a"
        startsWithVowel []    = False
        startsWithVowel (h:_) = h `elem` ['a','e','i','o','u']

the :: Object -> Text
the obj = pronoun <> " " <> obj ^. oTitle
  where pronoun = case obj ^. oNounType of
          Proper     -> ""
          Quantitive -> "the"
          Particular -> "the"

listItems :: Room -> State Story Text
listItems room = do
  let oids = room ^. rItems
  items <- mapM toObject oids
  let aItems = a <$> items
  let is = case items of
             item : _ | item ^. oIsPlural -> "are "
             _                            -> "is "
  return $ "There " <> is <> intercalate " and " aItems <> " here."

listExits :: Room -> State Story [Exit]
listExits room = do
  story <- get
  return $ buildExits story $ _rExitsBuilder room

-- TODO or keep name takeItem - but move helper methods into different module from story builder DSL
takeItemS :: Oid -> State Story ()
takeItemS oid = do
  story <- get
  let rId = story ^. sPlayer ^. pLocation
  modify $ (sPlayer . pInventory) %~ (oid :)
  modify $ (sRooms . at rId . _Just . rItems) %~ filter (/= oid)

goto :: Action (Maybe Rid) -> State Story (Either [Text] Rid) -- TODO revisit assumption that we either have txt to display, or a new room
goto roomAction = do
  (txts, mRid) <- runAction' roomAction
  case mRid of
    Just rid -> do modify $ \story -> (.~) (sPlayer . pLocation) rid story
                   return $ Right rid
    Nothing  -> return $ Left txts

lookupRoomAction :: Text -> State Story (Either Text (Action (Maybe Rid)))
lookupRoomAction exit = do
  s <- get
  let Just location = s ^. sRooms . at (s ^. sPlayer . pLocation)
      exits = buildExits s $ _rExitsBuilder location
      mRoomAction = _eRid <$> find ((== exit) . _eLabel) exits
  return $ justErr ("Could not find " <> exit <> " from " <> location ^. rTitle <> " exits are: " <> (show' $ map _eLabel exits)) mRoomAction

visible :: (Oid, Object) -> State Story Bool
visible (oid, _) = do
  s <- get
  let inRoom = oid `elem` s ^. sRooms . at (s ^. sPlayer . pLocation) ^. _Just . rItems
      inPossession = oid `elem` s ^. sPlayer .pInventory
  return $ inRoom || inPossession

lookupOidByTitle :: Text -> State Story (Either Text Oid)
lookupOidByTitle title = do
  s <- get
  visibleObjects <- filterM visible $ M.toList $ s ^. sObjects
  let vosWithTitle :: [(Oid, Object)]
      vosWithTitle = filter ((== title) . (^. oTitle) . snd) visibleObjects
      visibleTitles = (map (( ^. oTitle) . snd) visibleObjects)
  return $ fst <$> headErr ("no " <> title <> " visible (only: " <> show' visibleTitles <> ")") vosWithTitle

lookupSay :: Text -> State Story (Either Text (Action ()))
lookupSay say' = do
  s <- get
  let sayOptions = s ^. sSay
      mAction :: Maybe (Action ())
      mAction = snd <$> find ((== say') . fst) sayOptions
  return $ justErr ("Could not find say " <> say' <> " options are: " <> (show' $ map fst sayOptions)) mAction

use :: Oid -> Maybe Oid -> State Story [Text]
use oid1 mOid2 = trace ("use " <> show oid1 <> " with " <> show mOid2) $
  case mOid2 of
    Just oid2 -> useWith oid1 oid2
    Nothing   -> useItself oid1

useWith :: Oid -> Oid -> State Story [Text]
useWith oid1 oid2 = do
    s <- get
    obj1 <- toObject oid1
    obj2 <- toObject oid2
    let mAction = msum [ use' s obj2 oid1
                       , use' s obj1 oid2
                       ]
    case mAction of
      Nothing     -> return []
      Just action -> runAction action
  where
    use' :: Story -> Object -> Oid -> Maybe (Action ())
    use' s obj1' oid2' = do
      u <- toMaybe $ obj1' ^. oUse
      runUseAction s u oid2'
    toMaybe :: Either a b -> Maybe b
    toMaybe (Left _) = Nothing
    toMaybe (Right x) = Just x

useItself :: Oid -> State Story [Text]
useItself oid = do
    s <- get
    obj <- toObject oid
    let action = fromLeft $ obj ^. oUse
    runAction action
  where fromLeft :: Either a b -> a
        fromLeft (Left a) = a
        fromLeft (Right _) = error "Not right..."
