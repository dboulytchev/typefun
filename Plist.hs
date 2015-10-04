{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

-- A heterogenious list implementation.
-- Heterogenious list ["1", 2, (), [3, 4, 5]] can
-- be represented as a tree of right-associate nested pairs
-- ("1", (2, ((), ([3, 4, 5], ())))), where () plays role
-- of nil (empty list)

module Plist where

-- Functional relation: l is a type of hlist with head type h
-- and tail type t. 
-- hd, tl, len --- head, tail and length of a list
-- respectively.
class List l h t | l -> h t where
  hd    :: l -> h
  tl    :: l -> t
  len   :: l -> Integer

-- nil is a hlist
instance List () () () where
  hd    _ = error "Empty list"
  tl    _ = error "Empty list"
  len   _ = 0

-- If l is a type of list with head type h and tail type t, 
-- then (a, l) is a type of a list with head type a and
-- tail type l.
instance List l h t => List (a, l) a l where
  hd  = fst
  tl  = snd
  len = (+1) . len . snd

-- Concatenation (we need *both* data-function and type-function,
-- hence a separate class).
-- Concat is a functional relation, where a and b --- two
-- typea of hlists, c --- the type of their concatenation.
class Concat a b c | a b -> c where
  conc :: a -> b -> c

-- Base case.
instance Concat () l l where
  conc _ x = x

-- General case.
instance (List (h1, t1) h1 t1, 
          List l2 h2 t2, 
          Concat t1 l2 l3, 
          List (h1, l3) h1 l3) => 
          Concat (h1, t1) l2 (h1, l3) where  
  conc (h1, t1) l2 = (h1, conc t1 l2)

-- List reversion (accumulating version). 
-- A separate type class is needed as well.
class ReverseAcc a l r | a l -> r where
  reverse' :: a -> l -> r

instance ReverseAcc l () l where
  reverse' a _ = a

instance (List (h, t) h t, ReverseAcc (h, a) t r) => ReverseAcc a (h, t) r where
  reverse' a (h, t) = reverse' (h, a) t

-- List reversion.
class Reverse a b | a -> b where
  rev :: a -> b

instance Reverse () () where
  rev = id

instance ReverseAcc () a b => Reverse a b where
  rev = reverse' ()

-- Mapping hlists.
class GMap a b ta tb | a b ta -> tb where
  gmap :: (a -> b) -> ta -> tb

instance GMap a b () () where
  gmap f x = x

instance (List (a, ta) a ta, GMap a b ta tb, List (b, tb) b tb) => GMap a b (a, ta) (b, tb) where
  gmap f (a, ta) = (f a, gmap f ta)

class Ord a => Insert a l1 l2 | a l1 -> l2 where
  insert :: a -> l1 -> l2

instance Ord a => Insert a () (a, ()) where
  insert a () = (a, ())

instance (Ord a, List (a, t) a t, Insert a t (a, t)) => Insert a (a, t) (a, (a, t)) where
  insert a l@(x, xs) | a < x     = (a, l)
                     | otherwise = (x, insert a xs)

class Sort a where
  sort' :: a -> a

instance Sort () where
  sort' = id

instance (Ord a, Sort t, Insert a t (a, t)) => Sort (a, t) where
  sort' (a, t) = insert a (sort' t)