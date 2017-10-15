{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.RoomBuilder
  ( buildRoom
  ) where

import BasicPrelude
import Lens.Family                  ((.~))
import Control.Comonad.Store        (ComonadStore, Store, store, seeks)
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.Functor.Identity        (runIdentity)

import TextAd.Model.Core
import TextAd.Util.CoProduct ((*:*))
import TextAd.Util.Pairing   (pairEffect)


coSetRTitle :: ComonadStore Room w
            => w a
           -> CoSetRTitleF (w a)
coSetRTitle w = CoSetRTitle $ \title ->
  seeks (rTitle .~ title) w

coSetRDescr :: ComonadStore Room w
            => w a
            -> CoSetRDescrF (w a)
coSetRDescr w = CoSetRDescr $ \action ->
  seeks (rDescr .~ action) w

coSetRExits :: ComonadStore Room w
            => w a
            -> CoSetRExitsF (w a)
coSetRExits w = CoSetRExits $ \roomBuilder ->
  seeks (rExitsBuilder .~ roomBuilder) w

coSetRItems :: ComonadStore Room w
            => w a
            -> CoSetRItemsF (w a)
coSetRItems w = CoSetRItems $ \oids ->
  seeks (rItems .~ oids) w



type Stack                    = Store Room
type RoomBuilderInterpreter a = CofreeT CoRoomBuilderSyntax Stack a

mkCofree :: Stack a
         -> RoomBuilderInterpreter a
mkCofree =
  coiterT (   coSetRTitle
          *:* coSetRDescr
          *:* coSetRExits
          *:* coSetRItems
          )

interpret :: (a -> r -> c)
          -> RoomBuilderInterpreter a
          -> RoomBuilder r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter


buildRoom :: RoomBuilder r -> Room
buildRoom roomBuilder =
  let start                  :: Stack Room
      start                  = store id $ Room
                                 { _rTitle        = error "rTitle not defined"
                                 , _rDescr        = error "rDescr not defined"
                                 , _rExitsBuilder = return ()
                                 , _rItems        = []
                                 }
      roomBuilderInterpreter = mkCofree start
  in interpret (\l _ -> l) roomBuilderInterpreter roomBuilder
