{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module TypeDeleteCase where

data TTrue
data TFalse

class TypeEq x y b | x y -> b where
  eq :: x -> y -> b

instance TypeEq x x TTrue where
  eq = undefined

instance {-# OVERLAPPABLE #-} f ~ TFalse => TypeEq x y f where
  eq = undefined

class DeleteBy e l l' | e l -> l' where
  deleteBy :: e -> l -> l'

class DeleteCase b e h l l' | b e h l -> l' where
  deleteCase :: b -> e -> h -> l -> l'

instance DeleteBy e t t' => DeleteCase TTrue e e t t' where
  deleteCase b e h t = deleteBy e t

instance DeleteBy e t t' => DeleteCase TFalse e h t (h, t') where
  deleteCase b e h t = (h, deleteBy e t)

instance DeleteBy e () () where
  deleteBy _ = id

instance (TypeEq e h b, DeleteCase b e h t l') => DeleteBy e (h, t) l' where
  deleteBy e (h, t) = deleteCase (eq e h) e h t 

