{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}

module SYB.SelectFloat (selectFloat₂) where

import Data.Company
import Data.Generics

selectFloat₂ :: Company -> [Float]
selectFloat₂ = everything (++) ([] `mkQ` (: []))
