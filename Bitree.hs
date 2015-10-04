{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Bitree where

import Plist

class Bitree t a lt rt | t -> a lt rt where
  depth :: t -> Integer

instance Bitree () a lt rt where
  depth _ = 0

instance (Bitree l c ll lr, 
          Bitree r b rl rr) => Bitree (l, a, r) a l r where
  depth (l, _, r) = 1 + max (depth l) (depth r)

class ToList a b | a -> b where
  to_list :: a -> b

instance ToList () () where
  to_list = id

instance (Bitree (lt, a, rt) a lt rt, ToList lt ll, ToList rt rl, Concat ll rl l) => ToList (lt, a, rt) (a, l) where
  to_list (l, x, r) = (x, conc (to_list l) (to_list r))
