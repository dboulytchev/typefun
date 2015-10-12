{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

module Indexing where

data O   = O deriving (Show, Eq)
data S a = S a deriving (Show, Eq)

class ElementAt l i e | l i -> e where
  elementAt :: l -> i -> e

instance ElementAt (h, t) O h where
  elementAt (h, _) _ = h

instance ElementAt t i e => ElementAt (h, t) (S i) e where
  elementAt (_, t) (S i) = elementAt t i

class UpdateAt l i e m | l i e -> m where
  updateAt :: l -> i -> e -> m

instance UpdateAt (a, t) O b (b, t) where
  updateAt (_, t) O b = (b, t)

instance UpdateAt t i b t' => UpdateAt (h, t) (S i) b (h, t') where
  updateAt (a, t) (S i) b = (a, updateAt t i b)

x = elementAt (1, ())         O
y = elementAt (1, ("2", ())) (S O)

z = updateAt (1, ()) O "2"
t = updateAt (1, ([8], ())) (S O) []