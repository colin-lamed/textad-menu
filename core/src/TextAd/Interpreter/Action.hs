module TextAd.Interpreter.Action
  ( runAction
  , runAction'
  ) where

import BasicPrelude
import Control.Comonad.Cofree    (Cofree, coiter)
import Control.Monad.Trans.State (State, state)
import Data.DList                (DList, snoc)
import Data.Dynamic              (fromDynamic, toDyn)
import Lens.Family               ((%~), (^.))
import Lens.Family.Stock         (at, _Just)
import qualified Data.Map        as M
import qualified Data.DList      as DL

import TextAd.Model.Core
import TextAd.Util.CoProduct     ((*:*))
import TextAd.Util.Pairing       (pair)

--- Action interpreter into (DList Text, Story) ----------

coPrintLn :: (DList Text, Story) -> CoPrintLnF (DList Text, Story)
coPrintLn (txts, s) = CoPrintLn $ \txt ->
  (txts `snoc` txt, s)

coAddItem :: (DList Text, Story) -> CoAddItemF (DList Text, Story)
coAddItem (txts, s) = CoAddItem $ \rid oid ->
  let s2 = (sRooms . at rid . _Just . rItems) %~ (oid :) $ s
  in (txts, s2)

coTakeItem :: (DList Text, Story) -> CoTakeItemF (DList Text, Story)
coTakeItem (txts, s) = CoTakeItem $ \oid ->
  let s2 = (sPlayer . pInventory) %~ (oid :) $ s
  in (txts, s2)

coDestroyItem :: (DList Text, Story) -> CoDestroyItemF (DList Text, Story)
coDestroyItem (txts, s) = CoDestroyItem $ \oid ->
  let mrid = roomOf oid s
      update = case mrid of
                Just rid -> (sRooms . at rid . _Just . rItems) %~ (filter (/= oid))
                Nothing  -> (sPlayer . pInventory) %~ (filter (/= oid))
  in (txts, update s)

coIncScore :: (DList Text, Story) -> CoIncScoreF (DList Text, Story)
coIncScore (txts, s) = CoIncScore $ \i ->
  let s2 = sScore %~ (+ i) $ s
  in (txts, s2)

coSay :: (DList Text, Story) -> CoSayF (DList Text, Story)
coSay (txts, s) = CoSay $ \say atn ->
  let s2 = sSay %~ (\ss -> ss <> [(say, atn)]) $ s
  in (txts, s2)

coPlayerHas :: (DList Text, Story) -> CoPlayerHasF (DList Text, Story)
coPlayerHas (txts, s) = CoPlayerHas $ \oid ->
  let has = oid `elem` s ^. sPlayer ^. pInventory
  in (has, (txts, s))

coRoomHas :: (DList Text, Story) -> CoRoomHasF (DList Text, Story)
coRoomHas (txts, s) = CoRoomHas $ \rid oid ->
  let has = oid `elem` s ^. sRooms . at rid . _Just . rItems
  in (has, (txts, s))

roomOf :: Oid -> Story -> Maybe Rid
roomOf oid s =
  let rids = M.keys $ M.filter ((oid `elem`) . (^. rItems)) $ s ^. sRooms
      mHead (x : _) = Just x
      mHead _       = Nothing
  in mHead rids

coRoomOf :: (DList Text, Story) -> CoRoomOfF (DList Text, Story)
coRoomOf (txts, s) = CoRoomOf $ \oid ->
  (roomOf oid s, (txts, s))

currentRoom :: Story -> Rid
currentRoom s =
  s ^. sPlayer ^. pLocation

coCurrentRoom :: (DList Text, Story) -> CoCurrentRoomF (DList Text, Story)
coCurrentRoom (txts, s) = CoCurrentRoom $
  (currentRoom s, (txts, s))

coGetState :: (DList Text, Story) -> CoGetStateF (DList Text, Story)
coGetState (txts, s) = CoGetState $ \sid ->
  let val = case M.lookup (sidK sid) (s ^. sStates) of
          Just dyn -> case fromDynamic dyn of
                        Just val' -> val'
                        Nothing -> error' $ "could not extract value from" <> (sidK sid)
          Nothing -> error' $ "State " <> show' sid <> " has not been added to story"
  in (val, (txts, s))

coSetState :: (DList Text, Story) -> CoSetStateF (DList Text, Story)
coSetState (txts, s) = CoSetState $ \sid val ->
  let s2 = sStates %~ (M.insert (sidK sid) $ toDyn val) $ s
  in (txts, s2)

mkCofree :: (DList Text, Story) -> Cofree CoActionSyntax (DList Text, Story)
mkCofree =
  coiter (   coPrintLn
         *:* coAddItem
         *:* coTakeItem
         *:* coDestroyItem
         *:* coIncScore
         *:* coSay
         *:* coPlayerHas
         *:* coRoomHas
         *:* coRoomOf
         *:* coCurrentRoom
         *:* coGetState
         *:* coSetState
         )

interpret :: ((DList Text, Story) -> r -> c) -> (DList Text, Story) -> Action r -> c
interpret f start free =
  pair f (mkCofree start) free

runAction :: Action r -> State Story [Text]
runAction action =
  state $ \s ->
    let (dl, s') = interpret (\l _ -> l) (DL.empty, s) action
    in (DL.toList dl, s')

runAction' :: Action r -> State Story ([Text], r)
runAction' action =
  state $ \s ->
    let ((dl, s'), r) = interpret (\l r -> (l, r)) (DL.empty, s) action
    in ((DL.toList dl, r), s')
