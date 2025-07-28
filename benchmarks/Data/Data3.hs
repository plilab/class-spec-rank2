{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Data.Data3 (Data₃ (..)) where

import Data.Typeable (Proxy)

class (γ α) => Data₃ γ α where
  gmapT₃ :: Proxy γ -> (forall β. (Data₃ γ β) => β -> β) -> α -> α
  gmapQ₃ :: Proxy γ -> (forall β. (Data₃ γ β) => β -> ρ) -> α -> [ρ]
  gmapM₃ :: Proxy γ -> (Monad μ) => (forall β. (Data₃ γ β) => β -> μ β) -> α -> μ α

-- Basic instances
instance (γ Int) => Data₃ γ Int where
  gmapT₃ _ _ x = x
  gmapQ₃ _ _ _ = []
  gmapM₃ _ _ = return

instance (γ Float) => Data₃ γ Float where
  gmapT₃ _ _ x = x
  gmapQ₃ _ _ _ = []
  gmapM₃ _ _ = return

instance (γ Bool) => Data₃ γ Bool where
  gmapT₃ _ _ x = x
  gmapQ₃ _ _ _ = []
  gmapM₃ _ _ = return

instance (γ Char) => Data₃ γ Char where
  gmapT₃ _ _ x = x
  gmapQ₃ _ _ _ = []
  gmapM₃ _ _ = return

instance (γ [α], Data₃ γ α) => Data₃ γ [α] where
  gmapT₃ _ _ [] = []
  gmapT₃ _ f (x : xs) = f x : f xs
  gmapQ₃ _ _ [] = []
  gmapQ₃ _ f (x : xs) = [f x, f xs]
  gmapM₃ _ _ [] = return []
  gmapM₃ _ f (x : xs) = do
    x' <- f x
    xs' <- f xs
    return $ x' : xs'

instance (ctx Integer) => Data₃ ctx Integer where
  gmapT₃ _ _ x = x
  gmapQ₃ _ _ _ = []
  gmapM₃ _ _ = return

instance (γ (Maybe α), Data₃ γ α) => Data₃ γ (Maybe α) where
  gmapT₃ _ _ Nothing = Nothing
  gmapT₃ _ f (Just x) = Just $ f x
  gmapQ₃ _ _ Nothing = []
  gmapQ₃ _ f (Just x) = [f x]
  gmapM₃ _ _ Nothing = return Nothing
  gmapM₃ _ f (Just x) = Just <$> f x

instance (ctx (alp, bet), Data₃ ctx alp, Data₃ ctx bet) => Data₃ ctx (alp, bet) where
  gmapT₃ _ f (x, y) = (f x, f y)
  gmapQ₃ _ f (x, y) = [f x, f y]
  gmapM₃ _ f (x, y) = (,) <$> f x <*> f y
