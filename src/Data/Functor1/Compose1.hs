{-# language KindSignatures #-}
module Data.Functor1.Compose1 where

newtype
  Compose1
  (f :: (* -> *) -> (* -> *))
  (g :: (* -> *) -> (* -> *))
  (a :: * -> *)
  x
  = Compose1 { getCompose1 :: f (g a) x }
