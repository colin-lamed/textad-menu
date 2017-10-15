module TextAd.Util.Comonad
 ( coiter
 , tell
 ) where

import BasicPrelude
import Control.Comonad              (duplicate)
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Control.Comonad.Traced       (ComonadTraced, trace)

coiter :: Functor f => (a -> f a) -> a -> CofreeT f Identity a
coiter f a =
  (flip coiterT) (Identity a) $ \ia ->
    let a = runIdentity ia
    in map Identity (f a)

{-
import Control.Comonad.Store        (Store, store)
import Control.Comonad.Trans.Traced (TracedT(TracedT))
import Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import Data.DList                   (DList)
import Data.Functor.Identity        (Identity, runIdentity)
import qualified Data.DList         as DList

import TextAd.Model.Core
import TextAd.Interpreter.StateInterpreter (coGetState)
import TextAd.Util.CoProduct        ((*:*))
import TextAd.Util.Pairing          (pairEffect)
-}

-- | Add m to w a
tell :: ComonadTraced m w
          => m
          -> w a
          -> w a
tell m w =
  trace m (duplicate w)
