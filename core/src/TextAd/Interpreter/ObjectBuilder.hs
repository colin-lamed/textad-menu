module TextAd.Interpreter.ObjectBuilder
  ( buildObject
  ) where

import BasicPrelude
import Control.Comonad.Cofree (coiter)
import Lens.Family            ((.~))

import TextAd.Model.Core
import TextAd.Util.CoProduct  ((*:*))
import TextAd.Util.Pairing    (pair)

coSetOTitle :: Object -> CoSetOTitleF Object
coSetOTitle o = CoSetOTitle $ \title ->
  oTitle .~ title $ o

coSetONounType :: Object -> CoSetONounTypeF Object
coSetONounType o = CoSetONounType $ \nounType ->
  oNounType .~ nounType $ o

coSetOIsPlural :: Object -> CoSetOIsPluralF Object
coSetOIsPlural o = CoSetOIsPlural $ \isPlural ->
  oIsPlural .~ isPlural $ o

coSetODescr :: Object -> CoSetODescrF Object
coSetODescr o = CoSetODescr $ \action ->
  oDescr .~ action $ o

coSetOCanPickUp :: Object -> CoSetOCanPickUpF Object
coSetOCanPickUp o = CoSetOCanPickUp $ \canPickUp ->
  oCanPickUp .~ canPickUp $ o

coSetOUse :: Object -> CoSetOUseF Object
coSetOUse o = CoSetOUse $ \useAction ->
  oUse .~ useAction $ o

coSetOTalk :: Object -> CoSetOTalkF Object
coSetOTalk o = CoSetOTalk $ \action ->
  oTalk .~ Just action $ o

buildObject :: ObjectBuilder r -> Object
buildObject objectBuilder =
  let start = Object { _oTitle      = error "oTitle not defined" -- TODO enforce that this will definately be replaced - similarly, enforce that will only be set once. Applies to Room and Story too.
                     , _oNounType   = Particular
                     , _oIsPlural   = False
                     , _oDescr      = error "oDescr not defined"
                     , _oCanPickUp  = False
                     , _oUse        = return ()
                     , _oTalk       = Nothing
                     }
      cofree = coiter (   coSetOTitle
                      *:* coSetONounType
                      *:* coSetOIsPlural
                      *:* coSetODescr
                      *:* coSetOCanPickUp
                      *:* coSetOUse
                      *:* coSetOTalk
                      ) start
      free = objectBuilder
  in pair (\l _ -> l) cofree free
