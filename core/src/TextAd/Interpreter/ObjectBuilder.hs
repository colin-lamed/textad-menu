{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.ObjectBuilder
  ( buildObject
  ) where

import BasicPrelude
import Lens.Family                  ((.~))
import Control.Comonad.Store        (ComonadStore, Store, store, seeks)
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.Functor.Identity        (runIdentity)

import TextAd.Model.Core
import TextAd.Util.CoProduct ((*:*))
import TextAd.Util.Pairing   (pairEffect)


coSetOTitle :: ComonadStore Object w
            => w a
            -> CoSetOTitleF (w a)
coSetOTitle w = CoSetOTitle $ \title ->
  seeks (oTitle .~ title) w

coSetONounType :: ComonadStore Object w
               => w a
               -> CoSetONounTypeF (w a)
coSetONounType w = CoSetONounType $ \nounType ->
  seeks (oNounType .~ nounType) w

coSetOIsPlural :: ComonadStore Object w
               => w a
               -> CoSetOIsPluralF (w a)
coSetOIsPlural w = CoSetOIsPlural $ \isPlural ->
  seeks (oIsPlural .~ isPlural) w

coSetODescr :: ComonadStore Object w
            => w a
            -> CoSetODescrF (w a)
coSetODescr w = CoSetODescr $ \action ->
  seeks (oDescr .~ action) w

coSetOCanPickUp :: ComonadStore Object w
                => w a
                -> CoSetOCanPickUpF (w a)
coSetOCanPickUp w = CoSetOCanPickUp $ \canPickUp ->
  seeks (oCanPickUp .~ canPickUp) w

coSetOUse :: ComonadStore Object w
          => w a
          -> CoSetOUseF (w a)
coSetOUse w = CoSetOUse $ \useAction ->
  seeks (oUse .~ useAction) w

coSetOTalk :: ComonadStore Object w
           => w a
           -> CoSetOTalkF (w a)
coSetOTalk w = CoSetOTalk $ \action ->
  seeks (oTalk .~ Just action) w


type Stack = Store Object
type ObjectBuilderInterpreter a = CofreeT CoObjectBuilderSyntax Stack a

mkCofree :: Stack a
         -> ObjectBuilderInterpreter a
mkCofree =
  coiterT (   coSetOTitle
          *:* coSetONounType
          *:* coSetOIsPlural
          *:* coSetODescr
          *:* coSetOCanPickUp
          *:* coSetOUse
          *:* coSetOTalk
          )

interpret :: (a -> r -> c)
          -> ObjectBuilderInterpreter a
          -> ObjectBuilder r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter


buildObject :: ObjectBuilder r -> Object
buildObject objectBuilder =
  let start                    :: Stack Object
      start                    = store id $ Object
                                   { _oTitle      = error "oTitle not defined"
                                   , _oNounType   = Particular
                                   , _oIsPlural   = False
                                   , _oDescr      = error "oDescr not defined"
                                   , _oCanPickUp  = False
                                   , _oUse        = Right (return ())
                                   , _oTalk       = Nothing
                                   }
      objectBuilderInterpreter = mkCofree start
  in interpret (\l _ -> l) objectBuilderInterpreter objectBuilder
