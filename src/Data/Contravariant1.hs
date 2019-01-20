module Data.Contravariant1 where

class Contravariant1 f where
  contramap1 :: (a x -> b x) -> (f b x -> f a x)
