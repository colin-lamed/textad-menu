{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.UseAction
  ( runUseAction
  ) where

import BasicPrelude
import Control.Comonad.Store        (Store, store)
import Control.Comonad.Traced       (ComonadTraced)
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Control.Comonad.Trans.Traced (TracedT(TracedT))
import Data.Functor.Identity        (runIdentity)
import Data.Monoid                  (First(First, getFirst))

import TextAd.Model.Core
import TextAd.Util.Comonad          (tell)
import TextAd.Util.CoProduct        ((*:*))
import TextAd.Interpreter.StateInterpreter (coGetState)
import TextAd.Util.Pairing          (pairEffect)


coWith :: ComonadTraced (First (Action ())) w
       => Oid
       -> w a
       -> CoWithF (w a)
coWith oid w = CoWith $ \oid' a ->
  if oid == oid'
    then tell (First $ Just a) w
    else w

-- Note, First means we ignore any others if there's more than one
type Stack                  = TracedT (First (Action ())) (Store Story)
type UseActionInterpreter a = CofreeT CoUseActionSyntax Stack a

mkCofree :: Oid
         -> Stack a
         -> UseActionInterpreter a
mkCofree oid ts =
  coiterT (   (coWith oid)
          *:* coGetState
          ) ts

interpret :: (a -> r -> c)
          -> UseActionInterpreter a
          -> UseAction r
          -> c
interpret f interpreter =
    runIdentity . pairEffect f interpreter


runUseAction :: Story -> UseAction r -> Oid -> Maybe (Action ())
runUseAction s useAction oid =
    getFirst ma
  where
    start                :: Stack (First (Action ()), Story)
    start                = TracedT $ store (\s' ma' -> (ma', s')) s
    useActionInterpreter = mkCofree oid start
    (ma, _)              = interpret (\l _ -> l) useActionInterpreter useAction
