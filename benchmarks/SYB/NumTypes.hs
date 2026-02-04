{-# OPTIONS_GHC -O2 #-}

module SYB.NumTypes (numTypes₂) where

import Data.Expr
import Data.Generics

numTypes₂ :: Expr -> Int
numTypes₂ = everything (+) (0 `mkQ` my_q)

my_q :: Type -> Int
my_q _ = 1
