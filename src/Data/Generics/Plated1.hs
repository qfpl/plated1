{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Data.Generics.Plated1 where

import Data.Applicative1
import Data.Contravariant1
import Data.Functor1
import Data.Functor1.Const1
import Data.Functor1.Identity1
import Data.List1
import Data.Maybe1
import Data.Monad1

import Data.Functor.Product

type Transversal s t a b =
  forall f x. Applicative1 f => (a x -> f b x) -> (s x -> f t x)

type Foldversal s t a b =
  forall f x. (Applicative1 f, Contravariant1 f) =>
  (a x -> f b x) -> (s x -> f t x)

type Transversal' s a = Transversal s s a a
type Foldversal' s a = Foldversal s s a a

class Plated1 t where
  plate1 :: Transversal' t t

instance Plated1 (List1 a) where
  plate1 f (List1 l) =
    case l of
      [] -> pure1 (List1 [])
      a:as -> fmap1 (cons1 a) (f (List1 as))

{-# inline toListOf1 #-}
toListOf1 :: Foldversal s t a b -> s x -> [a x]
toListOf1 f s = unList1 $ getConst1 (f (\x -> Const1 $ List1 [x]) s)

{-# inline cosmosOf1 #-}
cosmosOf1 :: Foldversal a b a b -> Foldversal a b a c
cosmosOf1 d f s =
  fmap1 (\(Pair _ a) -> a) $
  tensor1 (Pair (f s) (d (cosmosOf1 d f) s))

{-# inline cosmos1 #-}
cosmos1 :: Plated1 a => Foldversal' a a
cosmos1 = cosmosOf1 plate1

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
    go = f . runIdentity1 . l (Identity1 . go)

{-# inline transformM1 #-}
transformM1
  :: forall m t y
   . (Plated1 t, Monad1 m)
  => (forall x. t x -> m t x)
  -> t y -> m t y
transformM1 = transformMOf1 plate1

{-# inline transformMOf1 #-}
transformMOf1
  :: forall m a b y
   . Monad1 m
  => Transversal a b a b
  -> (forall x. b x -> m b x)
  -> a y -> m b y
transformMOf1 l f = go
  where
    go :: forall x. a x -> m b x
    -- go t = l go t >>= f
    go t = f `bind1` l go t

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
   . (Plated1 t, Monad1 m)
  => (forall x. t x -> m (Maybe1 t) x)
  -> t y -> m t y
rewriteM1 = rewriteMOf1 plate1

{-# inline rewriteMOf1 #-}
rewriteMOf1
  :: forall m a b y
   . Monad1 m
  => Transversal a b a b
  -> (forall x. b x -> m (Maybe1 a) x)
  -> a y -> m b y
rewriteMOf1 l f = go
  where
    go :: forall x. a x -> m b x
    go = transformMOf1 l (\x -> maybe1 (pure1 x) go `bind1` f x)
