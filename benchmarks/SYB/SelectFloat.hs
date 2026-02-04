{-# OPTIONS_GHC -O2 #-}

module SYB.SelectFloat (selectFloat₂) where

import Data.Company
import Data.Generics

selectFloat₂ :: Company -> [Float]
selectFloat₂ = everything (++) ([] `mkQ` (: []))
