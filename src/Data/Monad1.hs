module Data.Monad1 where

import Data.Applicative1
import Data.Functor1
import Data.Functor1.Compose1

class Applicative1 f => Monad1 f where
  join1 :: Compose1 f f a x -> f a x

bind1 :: Monad1 m => (a x -> m b x) -> m a x -> m b x
bind1 f = join1 . Compose1 . fmap1 f
