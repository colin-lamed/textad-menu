{-# LANGUAGE TypeOperators
           , DeriveFunctor
           , MultiParamTypeClasses
           , FlexibleInstances
           , DataKinds
           , TypeFamilies
           , StandaloneDeriving
           , ExistentialQuantification
           , FlexibleContexts
           , UndecidableInstances
           #-}

-- Data Types a la Carte

module TextAd.Util.CoProduct where

import BasicPrelude
import Control.Applicative (liftA2)
import TextAd.Util.Pairing

data (f :+: g) a
  = InL (f a)
  | InR (g a)
  deriving (Functor)

infixr 8 :+:


class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj


data (f :*: g) a
  = Pair (f a) (g a)
  deriving (Functor)

infixr 9 :*:


instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (InL x) (Pair a _) = pair p x a
  pair p (InR x) (Pair _ b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (Pair a _) (InL x) = pair p a x
  pair p (Pair _ b) (InR x) = pair p b x

(*:*) :: (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Pair
infixr 9 *:*
