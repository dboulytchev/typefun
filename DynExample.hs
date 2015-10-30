{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}

module DynExample where

import DynType

rewrite :: Equal a b -> Equal (f a) (g a) -> Equal (f a) (g b)
rewrite ab faga = trans faga $ arg ab

data Type a = 
   Int  (Equal a Int) 
 | Bool (Equal a Bool)
 | Char (Equal a Char)
 | Unit (Equal a ())
 | forall x   . List (Equal a [x])      (Type x)
 | forall x y . Fun  (Equal a (x -> y)) (Type x) (Type y)
 | forall x y . Pair (Equal a (x, y))   (Type x) (Type y)

int :: Type Int
int = Int refl

bool :: Type Bool
bool = Bool refl

char :: Type Char
char = Char refl

unit :: Type ()
unit = Unit refl

list :: Type a -> Type [a]
list a = List refl a

infixr 5 .->.

(.->.) :: Type a -> Type b -> Type (a->b)
a .->. b = Fun refl a b

infixr 6 .*.

(.*.) :: Type a -> Type b -> Type (a, b)
a .*. b = Pair refl a b

instance TypeRepr Type where
  (Int  wa)    ~~ (Int  wb)    = Just $ (trans wa $ symm wb)
  (Bool wa)    ~~ (Bool wb)    = Just $ (trans wa $ symm wb)
  (Char wa)    ~~ (Char wb)    = Just $ (trans wa $ symm wb)
  (Unit wa)    ~~ (Unit wb)    = Just $ (trans wa $ symm wb)
  (List wa ta) ~~ (List wb tb) =
    case ta ~~ tb of
      Just w  -> Just $ trans (trans wa (DynType.list w)) (symm wb)
      Nothing -> Nothing    
  (Fun wa da ca) ~~ (Fun wb db cb) =
    case (da ~~ db, ca ~~ cb) of
      (Just wd, Just wc) -> Just $ trans (trans wa (rewrite wc (dom wd))) (symm wb)
      _                  -> Nothing     
  (Pair wa da ca) ~~ (Pair wb db cb) =
    case (da ~~ db, ca ~~ cb) of
      (Just wd, Just wc) -> Just $ trans (trans wa (rewrite wc (p0 wd))) (symm wb)
      _                  -> Nothing     
  _ ~~ _ = Nothing

class Traverse a where
  everywhere :: Dynamic Type -> a -> a

instance Traverse Int where  
  everywhere (f ::: Fun ab a b) n = 
    case (int ~~ a, b ~~ int) of 
      (Just wa, Just wb) -> coerce wb ((coerce ab f) (coerce wa n))
      _                  -> n

instance Traverse Bool where  
  everywhere (f ::: Fun ab a b) n = 
    case (bool ~~ a, b ~~ bool) of 
      (Just wa, Just wb) -> coerce wb ((coerce ab f) (coerce wa n))
      _                  -> n

instance Traverse Char where  
  everywhere (f ::: Fun ab a b) n = 
    case (char ~~ a, b ~~ char) of 
      (Just wa, Just wb) -> coerce wb ((coerce ab f) (coerce wa n))
      _                  -> n

instance Traverse () where  
  everywhere (f ::: Fun ab a b) n = 
    case (unit ~~ a, b ~~ unit) of 
      (Just wa, Just wb) -> coerce wb ((coerce ab f) (coerce wa n))
      _                  -> n

instance Traverse a => Traverse [a] where
  everywhere _ []     = []
  everywhere f (x:xs) = everywhere f x : everywhere f xs

instance (Traverse a, Traverse b) => Traverse (a, b) where
  everywhere f (a, b) = (everywhere f a, everywhere f b)

