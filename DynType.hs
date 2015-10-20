{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}  
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}

module DynType where

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

