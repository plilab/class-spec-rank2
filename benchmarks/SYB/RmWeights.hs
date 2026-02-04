{-# OPTIONS_GHC -O2 #-}

module SYB.RmWeights where

import Data.Generics
import Data.Tree

rmWeights₂ :: WTree Int Int -> WTree Int Int
rmWeights₂ = everywhere (mkT rmAdhoc)
  where
    rmAdhoc :: WTree Int Int -> WTree Int Int
    rmAdhoc (WithWeight t w) = t
    rmAdhoc t = t
