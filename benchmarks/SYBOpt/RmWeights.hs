{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

module SYBOpt.RmWeights where

import Data.Generics
import Data.Tree

rmWeights₄ :: WTree Int Int -> WTree Int Int
rmWeights₄ = everywhere (mkT rmAdhoc)
  where
    rmAdhoc :: WTree Int Int -> WTree Int Int
    rmAdhoc (WithWeight t _) = t
    rmAdhoc t = t
