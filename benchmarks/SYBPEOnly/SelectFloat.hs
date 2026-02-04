{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYBPEOnly.SelectFloat where

import Data.Company
import Data.Generics

selectFloat₃ :: Company -> [Float]
selectFloat₃ = everything (++) ([] `mkQ` (: []))
