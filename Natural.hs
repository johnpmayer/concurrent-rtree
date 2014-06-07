
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Natural where

data Nat = S Nat | Z
data VecNat :: Nat -> * -> * where
  Nil :: VecNat Z a
  Cons :: a -> VecNat n a -> VecNat (S n) a

singleton :: a -> VecNat (S Z) a
singleton = flip Cons Nil
