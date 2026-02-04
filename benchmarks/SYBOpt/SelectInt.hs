{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}

module SYBOpt.SelectInt where

import Data.Generics
import Data.Tree

selectInt₄ :: WTree Int Int -> [Int]
selectInt₄ = everything (++) ([] `mkQ` (: []))
