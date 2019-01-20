{-# language TypeOperators #-}
module Data.Applicative1 where

import Data.Functor.Product
import Data.Functor1
import Data.Function1

class Functor1 f => Applicative1 f where
  pure1 :: a x -> f a x
  tensor1 :: Product (f a) (f b) x -> f (Product a b) x

ap1 :: Applicative1 f => Product (f (a ~> b)) (f a) x -> f b x
ap1 p = fmap1 (\(Pair a b) -> apNat a b) (tensor1 p)
