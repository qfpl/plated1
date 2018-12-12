{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Data.Generics.Plated1 where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))

type Transversal s t a b =
  forall f y. Applicative f => (forall x. a x -> f (b x)) -> s y -> f (t y)

type Transversal' s a = Transversal s s a a

class Plated1 t where
  plate1 :: Transversal' t t

{-# inline transform1 #-}
transform1
  :: forall t y
   . Plated1 t
  => (forall x. t x -> t x)
  -> t y -> t y
transform1 = transformOf1 plate1

{-# inline transformOf1 #-}
transformOf1
  :: forall a b y
  . Transversal a b a b
  -> (forall x. b x -> b x)
  -> a y -> b y
transformOf1 l f = go
  where
    go :: forall x. a x -> b x
    go = f . runIdentity . l (Identity . go)

{-# inline transformM1 #-}
transformM1
  :: forall m t y
   . (Plated1 t, Monad m)
  => (forall x. t x -> m (t x))
  -> t y -> m (t y)
transformM1 = transformMOf1 plate1

{-# inline transformMOf1 #-}
transformMOf1
  :: forall m a b y
   . Monad m
  => Transversal a b a b
  -> (forall x. b x -> m (b x))
  -> a y -> m (b y)
transformMOf1 l f = go
  where
    go :: forall x. a x -> m (b x)
    go t = l go t >>= f

{-# inline rewrite1 #-}
rewrite1
  :: forall t y
   . Plated1 t
  => (forall x. t x -> Maybe (t x))
  -> t y -> t y
rewrite1 = rewriteOf1 plate1

{-# inline rewriteOf1 #-}
rewriteOf1
  :: forall a b y
   . Transversal a b a b
  -> (forall x. b x -> Maybe (a x))
  -> a y -> b y
rewriteOf1 l f = go
  where
    go :: forall x.  a x -> b x
    go = transformOf1 l (\x -> maybe x go (f x))

{-# inline rewriteM1 #-}
rewriteM1
  :: forall m t y
   . (Plated1 t, Monad m)
  => (forall x. t x -> m (Maybe (t x)))
  -> t y -> m (t y)
rewriteM1 = rewriteMOf1 plate1

{-# inline rewriteMOf1 #-}
rewriteMOf1
  :: forall m a b y
   . Monad m
  => Transversal a b a b
  -> (forall x. b x -> m (Maybe (a x)))
  -> a y -> m (b y)
rewriteMOf1 l f = go
  where
    go :: forall x. a x -> m (b x)
    go = transformMOf1 l (\x -> f x >>= maybe (return x) go)
