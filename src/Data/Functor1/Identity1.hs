module Data.Functor1.Identity1 where

import Data.Applicative1
import Data.Contravariant1
import Data.Functor1
import Data.Functor.Product

newtype Identity1 a x = Identity1 { runIdentity1 :: a x }

instance Functor1 Identity1 where
  fmap1 f (Identity1 a) = Identity1 (f a)

instance Applicative1 Identity1 where
  pure1 = Identity1
  tensor1 (Pair (Identity1 a) (Identity1 b)) = Identity1 (Pair a b)