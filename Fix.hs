module Fix where

newtype Fix f = In {out :: f (Fix f)}
