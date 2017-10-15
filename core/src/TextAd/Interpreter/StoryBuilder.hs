{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.StoryBuilder
  ( toStory
  ) where

import BasicPrelude
import Lens.Family                  ((.~), (%~))
import Control.Comonad.Store        (ComonadStore, Store, store, seeks)
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.Functor.Identity        (runIdentity)
import qualified Data.Map           as M

import TextAd.Model.Core
import TextAd.Interpreter.ObjectBuilder
import TextAd.Interpreter.RoomBuilder
import TextAd.Interpreter.StateInterpreter (coMkState)
import TextAd.Util.CoProduct               ((*:*))
import TextAd.Util.Pairing                 (pairEffect)


coSetSTitle :: ComonadStore Story w
            => w a
            -> CoSetSTitleF (w a)
coSetSTitle w = CoSetSTitle $ \title ->
  seeks (sTitle .~ title) w

coMkPlayer :: ComonadStore Story w
           => w a
           -> CoMkPlayerF (w a)
coMkPlayer w = CoMkPlayer $ \inventory location ->
  seeks (sPlayer .~ Player inventory location) w

coMkObject :: ComonadStore Story w
           => w a
           -> CoMkObjectF (w a)
coMkObject w = CoMkObject $ \oid ob ->
  let obj = buildObject ob
  in seeks (sObjects %~ (M.insert oid obj)) w

coMkRoom :: ComonadStore Story w
         => w a
         -> CoMkRoomF (w a)
coMkRoom w = CoMkRoom $ \rid rb ->
  let room = buildRoom rb
  in seeks (sRooms %~ (M.insert rid room)) w

coSetSInit :: ComonadStore Story w
           => w a
           -> CoSetSInitF (w a)
coSetSInit w = CoSetSInit $ \ma ->
  seeks (sInit .~ ma) w

coSetMaxScore :: ComonadStore Story w
              => w a
              -> CoSetMaxScoreF (w a)
coSetMaxScore w = CoSetMaxScore $ \i ->
  seeks (sMaxScore .~ Just i) w


type Stack                     = Store Story
type StoryBuilderInterpreter a = CofreeT CoStoryBuilderSyntax Stack a

mkCofree :: Stack a
         -> StoryBuilderInterpreter a
mkCofree =
  coiterT (   coSetSTitle
          *:* coMkPlayer
          *:* coMkObject
          *:* coMkRoom
          *:* coMkState
          *:* coSetSInit
          *:* coSetMaxScore
          )

interpret :: (a -> r -> c)
          -> StoryBuilderInterpreter a
          -> StoryBuilder r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter


toStory :: StoryBuilder () -> Story
toStory storyBuilder =
  let start                   :: Stack Story
      start                   = store id $ Story
                                          { _sTitle    = error "sTitle not defined"
                                          , _sPlayer   = error "sPlayer not defined"
                                          , _sObjects  = M.empty
                                          , _sRooms    = M.empty
                                          , _sStates   = M.empty
                                          , _sScore    = 0
                                          , _sSay      = []
                                          , _sInit     = Nothing
                                          , _sMaxScore = Nothing
                                          }
      storyBuilderInterpreter = mkCofree start
  in interpret (\l _ -> l) storyBuilderInterpreter storyBuilder
