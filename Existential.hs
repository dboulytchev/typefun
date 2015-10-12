{-# LANGUAGE ExistentialQuantification #-}

module Existential where

class Indexed a where
  index :: a -> Integer

data I = forall a . Indexed a => I a

sort' []     = []
sort' (x:xs) = insert x $ sort' xs where
  insert x [] = [x]
  insert x@(I a) l@(y@(I b):ys) | index a <= index b = x : l
                                | otherwise          = y : insert x ys