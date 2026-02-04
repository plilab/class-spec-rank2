{-# OPTIONS_GHC -O2 #-}

module SYB.SelectInt where

import Data.Generics
import Data.Tree

selectInt₂ :: WTree Int Int -> [Int]
selectInt₂ = everything (++) ([] `mkQ` (: []))
