module TextAd.Interpreter.StoryBuilder
  ( toStory
  ) where

import BasicPrelude
import Control.Comonad.Cofree    (coiter)
import Data.Dynamic              (toDyn)
import Lens.Family               ((.~), (%~))
import qualified Data.Map        as M

import TextAd.Model.Core
import TextAd.Interpreter.ObjectBuilder
import TextAd.Interpreter.RoomBuilder
import TextAd.Util.CoProduct     ((*:*))
import TextAd.Util.Pairing       (pair)

coSetSTitle :: Story -> CoSetSTitleF Story
coSetSTitle s = CoSetSTitle $ \title ->
  sTitle .~ title $ s

coMkPlayer :: Story -> CoMkPlayerF Story
coMkPlayer s = CoMkPlayer $ \inventory location ->
  sPlayer .~ Player inventory location $ s

coMkObject :: Story -> CoMkObjectF Story
coMkObject s = CoMkObject $ \oid ob ->
  let obj = buildObject ob
  in sObjects %~ (M.insert oid obj) $ s

coMkRoom :: Story -> CoMkRoomF Story
coMkRoom s = CoMkRoom $ \rid rb ->
  let room = buildRoom rb
  in sRooms %~ (M.insert rid room) $ s

coMkState :: Story -> CoMkStateF Story
coMkState s = CoMkState $ \l val ->
  let sid = Sid l -- we could generate label instead of client passing in (e.g. UUID)
      s' = sStates %~ (M.insert (sidK sid) (toDyn val)) $ s
  in (sid, s')

coSetSInit :: Story -> CoSetSInitF Story
coSetSInit s = CoSetSInit $ \ma ->
  sInit .~ ma $ s

coSetMaxScore :: Story -> CoSetMaxScoreF Story
coSetMaxScore s = CoSetMaxScore $ \i ->
  sMaxScore .~ Just i $ s

toStory :: StoryBuilder () -> Story
toStory storyBuilder =
  let start = Story { _sTitle    = error "sTitle not defined"
                    , _sPlayer   = error "sPlayer not defined"
                    , _sObjects  = M.empty
                    , _sRooms    = M.empty
                    , _sStates   = M.empty
                    , _sScore    = 0
                    , _sSay      = []
                    , _sInit     = Nothing
                    , _sMaxScore = Nothing
                    }
      cofree = coiter (   coSetSTitle
                      *:* coMkPlayer
                      *:* coMkObject
                      *:* coMkRoom
                      *:* coMkState
                      *:* coSetSInit
                      *:* coSetMaxScore
                      ) start
      free = storyBuilder
  in pair (\l _ -> l) cofree free
