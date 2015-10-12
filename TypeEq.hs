{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module TypeEq where

data TTrue
data TFalse

class TypeEq x y b | x y -> b where
  eq :: x -> y -> Bool

instance TypeEq x x TTrue where
  eq _ _ = True

instance {-# OVERLAPPABLE #-} f ~ TFalse => TypeEq x y f where
  eq _ _ = False

