{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

module SYBOpt.SelectInt where

import Data.Generics
import Data.Tree

selectInt₄ :: WTree Int Int -> [Int]
selectInt₄ = everything (++) ([] `mkQ` (: []))
