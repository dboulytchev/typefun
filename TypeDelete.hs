{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module TypeDelete where

class DeleteBy a e b | a e -> b where
  deleteBy :: e -> a -> b

instance DeleteBy () e () where
  deleteBy _ = id

instance DeleteBy t h t'' => DeleteBy (h, t) h t'' where
  deleteBy e (h, t) = deleteBy e t

instance {-# OVERLAPPABLE #-} (DeleteBy t e t', (h, t') ~ t'') => DeleteBy (h, t) e t'' where
  deleteBy e (h, t) = (h, deleteBy e t) 

x = deleteBy (1::Integer) (1::Integer, (2::Integer, ()))
y = deleteBy (1::Integer) (1::Integer, ("2", ([()], ())))
