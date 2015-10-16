{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ExistentialQuantification #-}

module Comparable where

import qualified GHC.Base
import qualified Data.List 
import qualified Data.Maybe

class ValueIndex a where
  valnum :: a -> Integer

instance ValueIndex Integer where
  valnum = id

instance ValueIndex Int where
  valnum = toInteger

instance ValueIndex Char where
  valnum = toInteger . GHC.Base.ord

fix f = f (fix f) 

primes = fix (\ s (x:xs) -> x : s [ y | y <- xs, y `rem` x /= 0]) [2..]

instance ValueIndex a => ValueIndex [a] where
  valnum = product . zipWith (^) primes . map valnum

data O   = O deriving Show
data S a = S a deriving Show

type Zero  = O
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three

n1 = S O
n2 = S n1
n3 = S n2
n4 = S n3

infixl 6 |+|
infixl 7 |*|
infixr 8 |^|

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

class Exp a b c | a b -> c where
  (|^|) :: a -> b -> c

instance Exp a O (S O) where
  _ |^| O = n1

instance (Exp a b ab, Mul ab a c) => Exp a (S b) c where
  a |^| (S b) = (a |^| b) |*| a

class Reify n where
  reify :: n -> Integer

instance Reify O where
  reify _ = 0

instance Reify n => Reify (S n) where
  reify (S n) = 1 + reify n

data Proxy t

toProxy :: t -> Proxy t
toProxy _ = undefined

elemProxy :: Proxy [a] -> Proxy a
elemProxy _ = undefined

class TypeNumber t n | t -> n where
  number :: Proxy t -> n

instance TypeNumber Integer One where
  number _ = n1

instance TypeNumber Int Two where
  number _ = n2

instance TypeNumber Char Four where
  number _ = n4

instance Mul Four Two m => TypeNumber Bool m where
  number _ = n4 |*| n2

instance (TypeNumber a n, Exp Three n m) => TypeNumber [a] m where
  number l = n3 |^| (number $ elemProxy l)

class TypeIndex t where
  typenum :: Proxy t -> Integer

instance (TypeNumber t n, Reify n) => TypeIndex t where
  typenum t = reify $ number t

class Index a where
   index :: a -> (Integer, Integer)

instance (TypeIndex a, ValueIndex a) => Index a where
   index a = (typenum (toProxy a), valnum a)

data I = forall a . (Show a, Index a) => I a

instance Show I where
  show (I a) = "I " ++ show a

sort' []     = []
sort' (x:xs) = insert x $ sort' xs where
  insert x [] = [x]
  insert x@(I a) l@(y@(I b):ys) | index a <= index b = x : l
                                | otherwise          = y : insert x ys
