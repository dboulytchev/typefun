{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}

module Exists where 

import GHC.Base
import qualified Plist

class Indexed a where  
  index :: a -> Integer

instance Indexed Integer where
  index = id

instance Indexed Bool where
  index x = if x then 1 else 0

instance Indexed Char where
  index = toInteger . ord 

fix f = f (fix f)
primes = fix (\ s (x:xs) -> x : s [y | y <- xs, y `rem` x /= 0]) [2..]

instance Indexed a => Indexed [a] where
  index = sum . zipWith (^) primes . map index

instance (Indexed a, Indexed b) => Indexed (a, b) where
  index (a, b) = index [index a, index b]

instance Indexed () where
  index _ = 0

class (Indexed a, Indexed l1) => Insert a l1 l2 | a l1 -> l2 where
  insert :: a -> l1 -> l2

instance Indexed a => Insert a () (a, ()) where
  insert a () = (a, ())

instance (Plist.List (a, t) a t, Insert b t (a, t)) => Insert a (a, t) (a, (a, t)) where
  insert a l@(x, xs) | index a < index x = (a, l)
                     | otherwise         = (x, insert a xs)

{-
class Indexed a => Sort a where
  sort' :: a -> a

instance Sort () where
  sort' = id

instance (Sort t, Insert a t (a, t)) => Sort (a, t) where
  sort' (a, t) = insert a (sort' t)
-}