{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module TextAd.Util.Pairing where

import BasicPrelude
import Data.Functor.Identity        (Identity(..), runIdentity)
import Control.Comonad.Trans.Cofree (CofreeT, runCofreeT)
import Control.Monad.Trans.Free     (FreeT, runFreeT)
import Control.Comonad.Cofree       (Cofree ((:<)))
import Control.Monad.Free           (Free (Pure, Free))
import qualified Control.Comonad.Trans.Cofree as Cofree
import qualified Control.Monad.Trans.Free     as Free



-- new typeclass: Pairing
class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

-- and some instances
instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  --pair p f g = p (snd f) (g (fst f))
  pair p f g = pair (flip p) g f

-- now we can create Pairing between Cofree f and Free g

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance Pairing f g => Pairing (CofreeT f Identity) (FreeT g Identity) where
  pair p c f  = z a b
    where z (a Cofree.:< _ ) (Free.Pure x)  = p a x
          z (_ Cofree.:< fs) (Free.Free gs) = pair (pair p) fs gs
          a = runIdentity $ runCofreeT c
          b = runIdentity $ runFreeT f
