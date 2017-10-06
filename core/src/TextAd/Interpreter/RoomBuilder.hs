module TextAd.Interpreter.RoomBuilder
  ( buildRoom
  ) where

import BasicPrelude
import Control.Comonad.Cofree    (coiter)
import Lens.Family               ((.~))

import TextAd.Model.Core
import TextAd.Util.CoProduct     ((*:*))
import TextAd.Util.Pairing       (pair)

coSetRTitle :: Room -> CoSetRTitleF Room
coSetRTitle r = CoSetRTitle $ \title ->
  rTitle .~ title $ r

coSetRDescr :: Room -> CoSetRDescrF Room
coSetRDescr r = CoSetRDescr $ \action ->
  rDescr .~ action $ r

coSetRExits :: Room -> CoSetRExitsF Room
coSetRExits r = CoSetRExits $ \roomBuilder ->
  rExitsBuilder .~ roomBuilder $ r

coSetRItems :: Room -> CoSetRItemsF Room
coSetRItems r = CoSetRItems $ \oids ->
  rItems .~ oids $ r


buildRoom :: RoomBuilder r -> Room
buildRoom roomBuilder =
  let start = Room { _rTitle        = error "rTitle not defined"
                   , _rDescr        = error "rDescr not defined"
                   , _rExitsBuilder = return ()
                   , _rItems        = []
                   }
      cofree = coiter (   coSetRTitle
                      *:* coSetRDescr
                      *:* coSetRExits
                      *:* coSetRItems
                      ) start
      free = roomBuilder
  in pair (\l _ -> l) cofree free
