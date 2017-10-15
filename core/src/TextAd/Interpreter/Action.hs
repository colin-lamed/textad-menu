{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.Action
  ( runAction
  , runAction'
  ) where

import BasicPrelude
import Control.Comonad.Store        (ComonadStore, Store, pos, seeks, store)
import Control.Comonad.Traced       (ComonadTraced)
import Control.Comonad.Trans.Traced (TracedT(TracedT))
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Control.Monad.Trans.State    (State, state)
import Data.DList                   (DList)

import Data.Functor.Identity        (runIdentity)
import Lens.Family                  ((%~), (^.))
import Lens.Family.Stock            (at, _Just)
import qualified Data.Map           as M
import qualified Data.DList         as DList

import TextAd.Model.Core
import TextAd.Interpreter.StateInterpreter (coGetState, coSetState)
import TextAd.Util.Comonad          (tell)
import TextAd.Util.CoProduct        ((*:*))
import TextAd.Util.Pairing          (pairEffect)


coPrintLn :: ComonadTraced (DList Text) w
          => w a
          -> CoPrintLnF (w a)
coPrintLn w = CoPrintLn $ \txt ->
  tell (DList.singleton txt) w

coAddItem :: ComonadStore Story w
          => w a
          -> CoAddItemF (w a)
coAddItem w = CoAddItem $ \rid oid ->
  seeks ((sRooms . at rid . _Just . rItems) %~ (oid :)) w

coTakeItem :: ComonadStore Story w
           => w a
           -> CoTakeItemF (w a)
coTakeItem w = CoTakeItem $ \oid ->
  seeks ((sPlayer . pInventory) %~ (oid :)) w

coDestroyItem :: ComonadStore Story w
              => w a
              -> CoDestroyItemF (w a)
coDestroyItem w = CoDestroyItem $ \oid ->
  let update   :: Story -> Story
      update s = case roomOf oid s of
                   Just rid -> ((sRooms . at rid . _Just . rItems) %~ (filter (/= oid))) s
                   Nothing  -> ((sPlayer . pInventory            ) %~ (filter (/= oid))) s
  in seeks update w

coIncScore :: ComonadStore Story w
           => w a
           -> CoIncScoreF (w a)
coIncScore w = CoIncScore $ \i ->
  seeks (sScore %~ (+ i)) w

coSay :: ComonadStore Story w
      => w a
      -> CoSayF (w a)
coSay w = CoSay $ \say atn ->
  seeks (sSay %~ (\ss -> ss <> [(say, atn)])) w

coPlayerHas :: ComonadStore Story w
            => w a
            -> CoPlayerHasF (w a)
coPlayerHas w = CoPlayerHas $ \oid ->
  let s   = pos w
      has = oid `elem` s ^. sPlayer ^. pInventory
  in (has, w)

coRoomHas :: ComonadStore Story w
          => w a
          -> CoRoomHasF (w a)
coRoomHas w = CoRoomHas $ \rid oid ->
  let s    = pos w
      has  = oid `elem` s ^. sRooms . at rid . _Just . rItems
  in (has, w)

roomOf :: Oid -> Story -> Maybe Rid
roomOf oid s =
  let rids = M.keys $ M.filter ((oid `elem`) . (^. rItems)) $ s ^. sRooms
      mHead (x : _) = Just x
      mHead _       = Nothing
  in mHead rids

coRoomOf :: ComonadStore Story w
         => w a
         -> CoRoomOfF (w a)
coRoomOf w = CoRoomOf $ \oid ->
  let s = pos w
  in (roomOf oid s, w)

currentRoom :: Story -> Rid
currentRoom s =
  s ^. sPlayer ^. pLocation

coCurrentRoom :: ComonadStore Story w
              => w a
              -> CoCurrentRoomF (w a)
coCurrentRoom w = CoCurrentRoom $
  let s = pos w
  in (currentRoom s, w)


type Stack               = TracedT (DList Text) (Store Story)
type ActionInterpreter a = CofreeT CoActionSyntax Stack a

mkCofree :: Stack a
         -> ActionInterpreter a
mkCofree =
  coiterT (   coPrintLn
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

interpret :: (a -> r -> c)
          -> ActionInterpreter a
          -> Action r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter

runAction :: Action r -> State Story [Text]
runAction action =
  state $ \s ->
    let start             :: Stack ([Text], Story)
        start             = TracedT $ store (\s' dl -> (DList.toList dl, s')) s
        actionInterpreter = mkCofree start
    in interpret (\l _ -> l) actionInterpreter action

runAction' :: Action r -> State Story ([Text], r)
runAction' action =
  state $ \s ->
    let start             :: Stack ([Text], Story)
        start             = TracedT $ store (\s' dl -> (DList.toList dl, s')) s
        actionInterpreter = mkCofree start
        ((txts, s'), r)   = interpret (\l r -> (l, r)) actionInterpreter action
    in ((txts, r), s')
