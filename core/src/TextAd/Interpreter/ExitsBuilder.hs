{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.ExitsBuilder
  ( buildExits
  ) where

import BasicPrelude
import Control.Comonad.Store        (Store, store)
import Control.Comonad.Traced       (ComonadTraced, trace)
import Control.Comonad.Trans.Traced (TracedT(TracedT))
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.DList                   (DList)
import Data.Functor.Identity        (runIdentity)
import qualified Data.DList         as DList

import TextAd.Model.Core
import TextAd.Interpreter.StateInterpreter (coGetState)
import TextAd.Util.Comonad          (tell)
import TextAd.Util.CoProduct        ((*:*))
import TextAd.Util.Pairing          (pairEffect)


coAddExit :: ComonadTraced (DList Exit) w
          => w a
          -> CoAddExitF (w a)
coAddExit w = CoAddExit $ \exit ->
  tell (DList.singleton exit) w


type Stack                     = TracedT (DList Exit) (Store Story)
type ExitsBuilderInterpreter a = CofreeT CoExitsBuilderSyntax Stack a

mkCofree :: Stack a
         -> ExitsBuilderInterpreter a
mkCofree =
  coiterT (coAddExit *:* coGetState)

interpret :: (a -> r -> c)
          -> ExitsBuilderInterpreter a
          -> ExitsBuilder r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter

buildExits :: Story -> ExitsBuilder r -> [Exit]
buildExits s exitsBuilder =
    exits
  where
    start                   :: Stack ([Exit], Story)
    start                   = TracedT $ store (\s' dl -> (DList.toList dl, s')) s
    exitsBuilderInterpreter = mkCofree start
    (exits, _) = interpret (\l _ -> l) exitsBuilderInterpreter exitsBuilder
