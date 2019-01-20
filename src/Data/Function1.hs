{-# language TypeOperators #-}
module Data.Function1 where

newtype (~>) f g x = Nat { apNat :: f x -> g x }
infixr 5 ~>