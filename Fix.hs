{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Fix where

newtype Fix f = In {out :: f (Fix f)}

foldFix :: (a -> (a -> Fix f -> b) -> f (Fix f) -> b) -> a -> Fix f -> b
foldFix f a x = f a (foldFix f) (out x)

data SG s = s :>: s | String ::= E
type S    = Fix SG

infixr 5 :>:
infix 6 ::=

interpret st s = foldFix int st s where
  int st int' (s1 :>: s2) = int' (int' st s1) s2
  int st int' (x  ::= e ) = (\ y -> if y == x then eval st e else st y)

data EG e = X String | C Int | e :+: e | e :*: e | e :-: e

infixl 5 :+:
infixl 5 :-:
infixl 6 :*:

type E = Fix EG

x :: String -> E
x = In . X

c :: Int -> E
c = In . C

infixl 5 |+|
infixl 5 |-|
infixl 6 |*|

(|+|) :: E -> E -> E
a |+| b = In $ a :+: b

(|*|) :: E -> E -> E
a |*| b = In $ a :*: b

(|-|) :: E -> E -> E
a |-| b = In $ a :-: b

z = eval (\_->8) $ c 2 |*| x "a" |+| x "b" |*| c 3

eval = foldFix fe where
  fe s f (X x)     = s x
  fe s f (C n)     = n
  fe s f (a :+: b) = f s a + f s b
  fe s f (a :*: b) = f s a * f s b
  fe s f (a :-: b) = f s a - f s b

instance Show E where
  show = foldFix fs "" where
    fs s f (X x) = s ++ x
    fs s f (C n) = s ++ show n
    fs s f (a :+: b) = f ((f s a) ++ " + ") b
    fs s f (a :-: b) = f ((f s a) ++ " - ") b
    fs s f (a :*: b) = f ((f s a) ++ " * ") b

data LG a e = Nil | Cons a e
type L a = Fix (LG a)

inj :: [a] -> L a
inj []     = In Nil
inj (x:xs) = In (Cons x (inj xs))

proj :: L a -> [a]
proj (In Nil) = []
proj (In (Cons x xs)) = x : proj xs

injE :: (e -> E) -> EG e -> E
injE f (X x) = In $ X x
injE f (C n) = In $ C n
injE f (x :+: y) = In $ f x :+: f y
injE f (x :-: y) = In $ f x :-: f y
injE f (x :*: y) = In $ f x :*: f y

{-
class InjE e where
  injE :: e -> E

instance {-# OVERLAPPING #-} InjE e => InjE (EG e) where
  injE (X x)     = In $ X x
  injE (C n)     = In $ C n
  injE (x :+: y) = In $ injE x :+: injE y

instance {-# OVERLAPPABLE #-} {- e --- free type variable => -} InjE e where
  injE = undefined
-}

