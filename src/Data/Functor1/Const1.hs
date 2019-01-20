{-# language KindSignatures #-}
module Data.Functor1.Const1 where

import Data.Applicative1
import Data.Contravariant1
import Data.Functor1
import Data.Monoid1

import Data.Functor.Product

newtype Const1 a (b :: * -> *) x = Const1 { getConst1 :: a x }

instance Functor1 (Const1 a) where
  fmap1 _ = \(Const1 a) -> Const1 a

instance Monoid1 a => Applicative1 (Const1 a) where
  pure1 = \x -> Const1 mempty1
  tensor1 = \(Pair (Const1 a) (Const1 b)) -> Const1 (mappend1 (Pair a b))

instance Contravariant1 (Const1 a) where
  contramap1 _ = \(Const1 a) -> Const1 a
