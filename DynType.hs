{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}

module DynType (Equal(Equal), TypeRepr((~~)), Dynamic((:::)), refl, trans, symm, list, p0, p1, arg, cod, dom, func, coerce, fromDyn) where

data Equal a b = Equal (forall f . f a -> f b)

refl :: Equal a a
refl = Equal id

trans :: Equal a b -> Equal b c -> Equal a c
trans (Equal ab) (Equal bc) = Equal $ bc . ab

subst :: (ta -> c a) -> (c b -> tb) -> Equal a b -> ta -> tb
subst lift sink (Equal f) = sink . f . lift

data Flip a b = Flip {unFlip :: Equal b a}

symm :: Equal a b -> Equal b a
symm ab = (subst Flip unFlip ab) refl

data List a b = List {unList :: Equal [a] [b]}

list :: Equal a b -> Equal [a] [b]
list ab = (subst List unList ab) refl

data P0 c a b = P0 {unP0 :: Equal (a, c) (b, c)}

p0 :: Equal a b -> Equal (a, c) (b, c)
p0 ab = (subst P0 unP0 ab) refl  

data P1 c a b = P1 {unP1 :: Equal (c, a) (c, b)}

p1 :: Equal a b -> Equal (c, a) (c, b)
p1 ab = (subst P1 unP1 ab) refl  

data Arg f a b = Arg {unArg :: Equal (f a) (f b)}

arg :: Equal a b -> Equal (f a) (f b)
arg ab = (subst Arg unArg ab) refl

cod :: Equal a b -> Equal (c -> a) (c -> b)
cod = arg

data Dom c a b = Dom {unDom :: Equal (a -> c) (b -> c)}

dom :: Equal a b -> Equal (a -> c) (b -> c)
dom ab = (subst Dom unDom ab) refl

data Func a f g = Func {unFunc :: Equal (f a) (g a)}

func :: Equal f g -> Equal (f a) (g a)
func fg = (subst Func unFunc fg) refl

data Id a = Id {unId :: a}

coerce :: Equal a b -> (a -> b)
coerce (Equal ab) = unId . ab . Id

class TypeRepr tr where
  (~~) :: tr a -> tr b -> Maybe (Equal a b)

data Dynamic tr = forall a . a ::: tr a

fromDyn :: TypeRepr tr => tr a -> Dynamic tr -> Maybe a
fromDyn e (x ::: a) = 
  case a ~~ e of
    Just w  -> Just $ coerce w x
    Nothing -> Nothing
               
