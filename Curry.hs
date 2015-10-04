{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

curry2 f = \ a b     -> f (a, b)
curry3 f = \ a b c   -> f (a, b, c)
curry4 f = \ a b c d -> f (a, b, c, d)

infixl 6 <<

(<<) :: ((a, b) -> c) -> a -> (b -> c)
f << x = \ y -> f (x, y)

succ' k f x = k $ f << x

one :: a -> a
one   = id
two   = succ' one
three = succ' two 

curry' n f = n f

{-
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

class Numeral n a b | n -> a b where
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

zipWith10 = zipWith' (num n10)
zipWith80 = zipWith' (num $ n10 |*| n8)
-}