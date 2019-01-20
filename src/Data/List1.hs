module Data.List1 where

import Data.Monoid1
import Data.Functor.Product

newtype List1 f a = List1 { unList1 :: [f a] }

nil1 :: List1 f a
nil1 = List1 []

cons1 :: f a -> List1 f a -> List1 f a
cons1 a (List1 b) = List1 (a : b)

list1 :: r a -> (f a -> r a -> r a) -> List1 f a -> r a
list1 n c = go
  where
    go (List1 l) =
      case l of
        [] -> n
        a:as -> c a (go $ List1 as)

instance Monoid1 (List1 a) where
  mempty1 = nil1
  mappend1 = \(Pair (List1 a) (List1 b)) -> List1 (a <> b)