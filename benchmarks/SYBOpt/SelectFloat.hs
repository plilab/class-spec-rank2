{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}

module SYBOpt.SelectFloat where

import Data.Company
import Data.Generics

selectFloat₄ :: Company -> [Float]
selectFloat₄ = everything (++) ([] `mkQ` (: []))
