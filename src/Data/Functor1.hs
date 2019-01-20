module Data.Functor1 where

class Functor1 f where
  fmap1 :: (a x -> b x) -> (f a x -> f b x)
