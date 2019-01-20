module Data.Monoid1 where

import Data.Functor.Product

class Monoid1 a where
  mempty1 :: a x
  mappend1 :: Product a a x -> a x
