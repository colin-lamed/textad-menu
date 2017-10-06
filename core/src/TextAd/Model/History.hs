
-- |
-- Supports serialising and deserialsing a History of actions, which can then
-- be replayed.

module TextAd.Model.History
  ( History
  -- * Update history
  , addGo, addTake, addExamine, addUse, addTalk, addSay
  -- * Serialise history
  , serialise, deserialse
  -- * Replay history
  , replayAll
  , initStory
  ) where

import BasicPrelude              hiding ((<|>), try)
import Control.Monad.Trans.State (get, modify, State)
import Lens.Family               ((^.), (.~))
import Text.Parsec               (many, manyTill, noneOf, parse, sepBy, string, try, (<|>), Parsec)
import Debug.Trace               (trace)
import qualified Data.Text       as T

import TextAd.Model.Core
import TextAd.Model.Util
import TextAd.Interpreter.Action

data HistoryEntry
  = HGo Text
  | HTake Text
  | HExamine Text
  | HUse Text Text
  | HTalk Text
  | HSay Text
  deriving (Show)

type History = [HistoryEntry]

trace' :: Text -> a -> a
trace' = trace . T.unpack

-- | Add go instruction to 'History'.
addGo :: Text -> History -> History
addGo t = ((HGo t) :)

-- | Add take instruction to 'History'.
addTake :: Text -> History -> History
addTake t = ((HTake t) :)

-- | Add examine instruction to 'History'.
addExamine :: Text -> History -> History
addExamine t = ((HExamine t) :)

-- | Add use instruction to 'History'.
addUse :: Text -> Text -> History -> History
addUse t1 t2 = ((HUse t1 t2) :)

-- | Add talk instruction to 'History'.
addTalk :: Text -> History -> History
addTalk t = ((HTalk t) :)

-- | Add say instruction to 'History'.
addSay :: Text -> History -> History
addSay t = ((HSay t) :)

replay :: (Either Text [Text]) -> HistoryEntry -> State Story (Either Text [Text])
replay _ (HGo dir) = trace' ("HGo " <> dir) $ do
  eRoomAction <- lookupRoomAction dir
  case eRoomAction of
    Right roomAction -> do
      eRid <- goto roomAction
      case eRid of
        Left  txt -> return $ Right txt
        Right rid -> do room <- toRoom rid
                        txt  <- runAction $ room ^. rDescr
                        return $ Right txt
    Left err -> return $ Left err

replay _ (HTake oName) = trace' ("HTake " <> oName) $ do
  eOid <- lookupOidByTitle oName
  case eOid of
    Right oid -> do
                 _ <- takeItemS oid
                 return $ Right []
    Left err -> return $ Left err

replay _ (HExamine oName) = trace' ("HExamine " <> oName) $ do
  eOid <- lookupOidByTitle oName
  case eOid of
    Right oid -> do
      o   <- toObject oid
      txt <- runAction $ o ^. oDescr
      return $ Right txt
    Left err -> return $ Left err

replay _ (HUse oName1 oName2) = trace' ("HUse " <> oName1 <> " `with` " <> oName2) $ do
  eOid1 <- lookupOidByTitle oName1
  eOid2 <- lookupOidByTitle oName2
  case (eOid1, eOid2) of
    (Right oid1, Right oid2) -> Right <$> use oid1 oid2
    (Left err,   _         ) -> return $ Left err
    (_,          Left err  ) -> return $ Left err

replay _ (HTalk who) = trace' ("HTalk " <> who) $ do
  modify $ sSay .~ []
  eOid <- lookupOidByTitle who
  case eOid of
    Right oid -> do
      o <- toObject oid
      let Just action = o ^. oTalk
      txt <- runAction action
      return $ Right txt
    Left err -> return $ Left err

replay _ (HSay say') = trace' ("HSay " <> say') $ do
  eAtn <- lookupSay say'
  case eAtn of
    Right atn -> do
      modify $ sSay .~ []
      txt <- runAction atn
      return $ Right txt
    Left err -> return $ Left err

historiesP ::  Parsec String () History
historiesP = sepBy historyP (string "\n")
  where
  g :: Parsec String () HistoryEntry
  g  = HGo      <$> (string "go "      >> many (noneOf "\n") >>= return . T.pack)
  t  = HTake    <$> (string "take "    >> many (noneOf "\n") >>= return . T.pack)
  e  = HExamine <$> (string "examine " >> many (noneOf "\n") >>= return . T.pack)
  tt = HTalk    <$> (string "talk to " >> many (noneOf "\n") >>= return . T.pack)
  s  = HSay     <$> (string "say "     >> many (noneOf "\n") >>= return . T.pack)
  u  = string "use " >> HUse <$> (manyTill (noneOf "\n") (try $ string " with ") >>= return . T.pack)
                             <*> (many (noneOf "\n") >>= return . T.pack)
  historyP = (try g) <|> (try t) <|> (try e) <|> (try u) <|> (try tt) <|> (try s)

replay' :: (History, Either Text [Text]) -> HistoryEntry -> State Story (History, Either Text [Text])
replay' (hs, e@(Right txts)) h = do
  res <- replay e h
  return $ case res of
    Right txts' -> (h : hs, Right (txts <> txts'))
    Left  err   -> (hs,     Left err)
replay' (hs, e) _ = return $ (hs, e)

-- | Replay 'Text' of history on 'Story'.
--
-- If it fails, it will still have updated the story as far as it could, and
-- return the portion of history which was successfully applied, as well as an
-- error message.
replayAll :: Text -> State Story (History, Either Text [Text])
replayAll ser =
  case deserialse ser of
    Right histories -> foldM replay' ([], Right []) (reverse histories)
    Left  err       -> return $ ([], Left err)


toText :: HistoryEntry -> Text
toText (HGo     t)     = "go "      <> t
toText (HTake   t)     = "take "    <> t
toText (HExamine t)    = "examine " <> t
toText (HUse    t1 t2) = "use " <> t1 <> " with " <> t2
toText (HTalk   t)     = "talk to " <> t
toText (HSay    t)     = "say "     <> t

-- | Convert 'History' into 'Text' representation
serialise :: History -> Text
serialise = intercalate "\n" . fmap toText

-- | Convert 'Text' representation back into 'History'
deserialse :: Text -> Either Text History
deserialse = either (Left . show') Right . parse historiesP "" . T.unpack

--------------------------------------------------------------------------------

initStory :: Text -> State Story (Either (History, Text) ([Text], [Text]))
initStory path = do
  s <- get
  initTxt <- case s ^. sInit of
    Just atn -> runAction atn
    Nothing  -> return []
  let initTxt' = if T.null path then initTxt else []
  res <- replayAll path
  trace' ("replayAll returned: " <> show' res) $ return $ case res of
    (_, Right historicTxt) -> Right (historicTxt, initTxt')
    (h, Left err)          -> Left (h, err)
