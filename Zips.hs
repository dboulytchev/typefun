{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Zips where

infixl 6 <<

(<<) :: [a -> b] -> [a] -> [b]

(f:fs) << (x:xs) = f x : fs << xs
_      << _      = []

zipWith' n f = n $ repeat f

data O   = O deriving Show
data S a = S a deriving Show

infixl 6 |+|
infixl 7 |*|

class Add a b c | a b -> c where
  (|+|) :: a -> b -> c

instance Add O a a where
  (|+|) _ = id

instance Add a b c => Add (S a) b (S c) where
  (S x) |+| y = S (x |+| y)

class Mul a b c | a b -> c where
  (|*|) :: a -> b -> c

instance Mul O a O where
  _ |*| _ = O

instance (Mul a b ab, Add b ab c) => Mul (S a) b c where
  (S a) |*| b = b |+| a |*| b

class Numeral n a b | n a -> b where
  num :: n -> a -> b

instance Numeral O [a] [a] where
  num _ = id

instance Numeral n [b] c => Numeral (S n) [a->b] ([a]->c) where
  num (S n) fs xs = num n $ fs << xs

n1  = S O
n2  = S n1
n3  = S n2
n4  = n2 |*| n2
n5  = n3 |+| n2
n6  = n3 |*| n2
n7  = S n6 
n8  = n4 |*| n2
n9  = n5 |+| n4
n10 = n5 |*| n2

zipWith10 f = zipWith' (num n10) f
zipWith80 f = zipWith' (num $ n10 |*| n8) f
zipWith4  f = zipWith' (num n4) f
