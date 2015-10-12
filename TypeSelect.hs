{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}  
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeSelect where

data O   = O deriving (Show, Eq)
data S a = S a deriving (Show, Eq)

class SelectBy a e where
  selectBy :: (e -> Bool) -> a -> [e]

instance SelectBy () e where
  selectBy _ _ = []

instance {-# OVERLAPPABLE #-} SelectBy t e => SelectBy (a, t) e where
  selectBy p (a, t) = selectBy p t 

instance SelectBy t e => SelectBy (e, t) e where
  selectBy p (a, t) = if p a then a : selectBy p t else selectBy p t

x = selectBy (=="1") ()
y = selectBy (=="3") ([()], ("3", (["4"], ("5", ()))))

class ReplaceIn e l where
  replaceIn :: (e -> e) -> l -> l

instance ReplaceIn e () where
  replaceIn _ = id

instance {-# OVERLAPPABLE #-} ReplaceIn e t => ReplaceIn e (h, t) where
  replaceIn r (h, t) = (h, replaceIn r t)

instance ReplaceIn e t => ReplaceIn e (e, t) where
  replaceIn r (h, t) = (r h, replaceIn r t)

z = replaceIn (\ _ -> "1") ()
t = replaceIn (\ _ -> "1") ([()], ("3", (["4"], ("5", ()))))

class Contains e l where
  contains :: (e -> Bool) -> l -> Bool

instance Contains e () where
  contains _ _ = False

instance {-# OVERLAPPABLE #-} Contains e t => Contains e (h, t) where
  contains p (_, t) = contains p t

instance Contains e t => Contains e (e, t) where
  contains p (e, t) = p e || contains p t

a = contains (=="3") ()
b = contains (=="3") (2::Integer, ("3", ([()], ())))

replaceTest l =  
  let selType y x = True || x == y in
  let string = selType ""          in
  let int    = selType (1::Integer) in
  let l'     = if contains string l  then replaceIn (++"!")         l  else l in
  let l''    = if contains int    l' then replaceIn (+(1::Integer)) l' else l' in
  l''
  