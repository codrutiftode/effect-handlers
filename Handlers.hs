{- Open handlers -}

{-# LANGUAGE TypeFamilies,
    MultiParamTypeClasses,
    GADTs,
    TypeOperators,
    RankNTypes,
    FunctionalDependencies,
    PolyKinds
  #-}

module Handlers where

import Data.Kind (Type)

type family Return (opApp :: Type) :: Type
type family Result (h :: Type) :: Type
class ((h :: Type) `Handles` (op :: j -> k -> Type)) (e :: j) | h op -> e where
  clause :: op e u -> (Return (op e u) -> h -> Result h) -> h -> Result h
newtype Comp h a = Comp {handle :: (a -> h -> Result h) -> h -> Result h}
doOp :: (h `Handles` op) e => op e u -> Comp h (Return (op e u))
doOp op = Comp (\k h -> clause op k h)
-- doOp op = Comp (\k -> clause op k)
-- doOp op = Comp (clause op)
-- doOp    = Comp . clause
-- We are careful not to use this equivalent implementation because it
-- leads to an enormous slow-down. Pointless programming in GHC is
-- dangerous!
--
-- doOp = Comp . clause

instance Applicative (Comp h) where
  pure v = Comp (\k h -> k v h)
  (<*>) :: Comp h (a -> b) -> Comp h a -> Comp h b
-- Writing out (now reduce):
-- f <*> x = f >>= \f' ->
--     x >>= \x' ->
--         return (f' x')
  Comp f <*> Comp y' =
    Comp (\k h -> f (\x h' ->
      handle ((\f' ->
        Comp (\k' h -> y' (\z h'' ->
          handle (return (f' z))
            k' h'') h)) x) k h') h)

instance Monad (Comp h) where
  Comp c >>= f = Comp (\k h -> c (\x h' -> handle (f x) k h') h)

instance Functor (Comp h) where
  fmap f c = c >>= \x -> return (f x)
