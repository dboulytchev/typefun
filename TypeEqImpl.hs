{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}

module TypeEqImpl where

import Plist

data O = O
data S a = S a

type One   = S O
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four
type Six   = S Five

one :: One
one = S O

two :: Two
two = S one

three :: Three
three = S two

four :: Four
four = S three

five :: Five
five = S four

data TTrue
data TFalse

class EqRepr x y where
  convert :: x -> y

instance EqRepr O O where
  convert = id

instance EqRepr Int Int where
  convert = id

instance EqRepr Char Char where
  convert = id

instance EqRepr Bool Bool where
  convert = id

instance EqRepr () () where 
  convert = id

instance EqRepr a b => EqRepr [a] [b] where
  convert = map convert

instance EqRepr x y => EqRepr (S x) (S y) where
  convert (S x) = S (convert x)

instance (EqRepr x y, EqRepr p q) => EqRepr (x, p) (y, q) where
  convert (x, p) = (convert x, convert p)

class TypeRepr a r | a -> r where
  pack   :: a -> r
  unpack :: r -> a

instance TypeRepr Int (O, (Int, ())) where
  pack x = (O, (x, ()))
  unpack (_, (x, _)) = x

instance TypeRepr Char (One, (Char, ())) where
  pack x = (one, (x, ()))
  unpack (_, (x, _)) = x

instance TypeRepr Bool (Two, (Bool, ())) where
  pack x = (two, (x, ()))
  unpack (_, (x, _)) = x

instance TypeRepr () (Three, ((), ())) where
  pack x = (three, (x, ()))
  unpack (_, (x, _)) = x

instance (TypeRepr a ar, TypeRepr b br) => TypeRepr (a, b) (Four, ((a, b), (ar, (br, ())))) where
  pack (a, b) = (four, ((a, b), (pack a, (pack b, ()))))
  unpack (_, (x, _)) = x

element :: [a] -> a
element = undefined

instance (TypeRepr a ar) => TypeRepr [a] (Five, ([a], (ar, ()))) where
  pack a = (five, (a, (pack $ element a, ())))
  unpack (_, (x, _)) = x

class TypeEq a b where
  coerce :: a -> b

instance (TypeRepr a ar, TypeRepr b br, EqRepr ar br) => TypeEq a b where
  coerce = unpack . convert . pack

tmap :: TypeEq a b => (b -> c) -> a -> c
tmap f x = f $ coerce x
