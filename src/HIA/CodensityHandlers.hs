{- Handlers using a free monad and the codensity monad -}

{-# LANGUAGE TypeFamilies,
    MultiParamTypeClasses,
    GADTs,
    TypeOperators,
    RankNTypes,
    FunctionalDependencies,
    PolyKinds
  #-}

module HIA.CodensityHandlers where

import Data.Kind (Type)

type family Return (opApp :: Type) :: Type
type family Result (h :: Type) :: Type
class ((h :: Type) `Handles` (op :: j -> k -> Type)) (e :: j) | h op -> e where
  clause :: op e u -> (Return (op e u) -> h -> Result h) -> h -> Result h

newtype Comp h a = Comp {unComp :: forall r . (a -> RawComp h r) -> RawComp h r}
data RawComp h a where
  Ret :: a -> RawComp h a
  Do  :: (h `Handles` op) e => op e u -> (Return (op e u) -> RawComp h a) -> RawComp h a

instance Functor (RawComp h) where
  fmap f (Ret a) = Ret (f a)
  fmap f (Do op k) = Do op (\x -> fmap f (k x))

instance Applicative (RawComp h) where
  pure = Ret
  (Ret f) <*> (Ret v) = Ret (f v)
  (Ret f) <*> (Do op kv) = Do op (\x -> fmap f (kv x))
  (Do op kf) <*> v = Do op (\x -> kf x <*> v)

instance Monad (RawComp h) where
  Ret v   >>= f = f v
  Do op k >>= f = Do op (\x -> k x >>= f)

instance Functor (Comp h) where
  fmap f (Comp c) = Comp (\k -> c (\x -> k (f x)))

instance Applicative (Comp h) where
  pure v = Comp (\k -> k v)
  (Comp f) <*> (Comp v) = Comp (\kb -> f (\f' -> v (\a' -> kb (f' a'))))

instance Monad (Comp c) where
  Comp c >>= f = Comp (\k -> c (\x -> unComp (f x) k))

doOp :: (h `Handles` op) e => op e u -> Comp h (Return (op e u))
doOp op = Comp (\k -> Do op k)

handle :: Comp h a -> (a -> h -> Result h) -> h -> Result h
handle (Comp c) = handle' (c return)
  where
    handle' :: RawComp h a -> (a -> h -> Result h) -> h -> Result h
    handle' (Ret v) r h = r v h
    handle' (Do op k) r h = clause op (\v h' -> handle' (k v) r h') h
