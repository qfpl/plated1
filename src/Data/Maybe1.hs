module Data.Maybe1 where

newtype Maybe1 f a = Maybe1 { unMaybe1 :: Maybe (f a) }

maybe1 :: r x -> (a x -> r x) -> Maybe1 a x -> r x
maybe1 a b = maybe a b . unMaybe1