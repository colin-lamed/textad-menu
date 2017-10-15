{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module TextAd.Util.Pairing where

import BasicPrelude
import Data.Functor.Identity        (Identity(..), runIdentity)
import Control.Comonad.Trans.Cofree (CofreeT, runCofreeT, unwrap, Cofree, CofreeF ((:<)))
import Control.Monad.Trans.Free     (FreeT, runFreeT, Free, FreeF (Pure, Free))
import Control.Comonad              (Comonad, extract)


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

instance Pairing f g => Pairing (CofreeT f Identity) (FreeT g Identity) where
  pair p c f  = z a b
    where z (a :< _ ) (Pure x)  = p a x
          z (_ :< fs) (Free gs) = pair (pair p) fs gs
          a = runIdentity $ runCofreeT c
          b = runIdentity $ runFreeT f


pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r)
           -> CofreeT f w a
           -> FreeT g m b
           -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x  -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs
