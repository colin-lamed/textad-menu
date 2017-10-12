-- |
-- DSL for building 'Story'.
module TextAd.Story
  ( StoryDef
  , Rid(..), Oid(..), Sid(..)
  , Story(..), Player(..), Room(..), Object(..), DirHint(..)
  , NounType(..)
  , setSTitle, mkPlayer, mkRoom, mkObject, mkState, setSInit, setMaxScore
  , setRTitle, setRDescr, setRExits, setRItems
  , setOTitle, setONounType, setOIsPlural, setODescr, setOCanPickUp, setOUse, talkO
  , printLn, addItem, takeItem, destroyItem, incScore, say, getState, setState, playerHas, roomHas, roomOf, currentRoom
  , exit, action
  , with
  -- for convenience
  , module BasicPrelude
  ) where


import BasicPrelude
import TextAd.Model.Core
import TextAd.Model.Dsl
