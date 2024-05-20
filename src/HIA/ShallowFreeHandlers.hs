{- Shallow handlers using a free monad -}

{-# LANGUAGE TypeFamilies, RankNTypes,
    MultiParamTypeClasses, FlexibleContexts,
    TypeOperators, GADTs, FunctionalDependencies, PolyKinds #-}

module HIA.ShallowFreeHandlers where

import Data.Kind (Type)

type family Return (opApp :: Type) :: Type
type family Result (h :: Type) :: Type
type family Inner h :: Type

class ((h :: Type) `Handles` (op :: j -> k -> Type)) (e :: j) | h op -> e where
  clause :: op e u -> (Return (op e u) -> Comp h (Inner h)) -> h -> Result h

data Comp h a where
  Ret :: a -> Comp h a
  Do  :: ((h `Handles` op) e) => op e u -> (Return (op e u) -> Comp h a) -> Comp h a

instance Functor (Comp h) where
  fmap f (Ret a) = Ret (f a)
  fmap f (Do op k) = Do op (\x -> fmap f (k x))

instance Applicative (Comp h) where
  pure = Ret
  (Ret f) <*> (Ret v) = Ret (f v)
  (Ret f) <*> (Do op kv) = Do op (\x -> fmap f (kv x))
  (Do op kf) <*> v = Do op (\x -> kf x <*> v)

instance Monad (Comp h) where
  Ret v   >>= f = f v
  Do op k >>= f = Do op (\x -> k x >>= f)

doOp :: (h `Handles` op) e => op e u -> Comp h (Return (op e u))
doOp op = Do op return

handle :: Comp h (Inner h) -> (Inner h -> h -> Result h) -> h -> Result h
handle (Ret v) r h   = r v h
handle (Do op k) r h = clause op k h

-- pure handlers
-- data PureHandler a = PureHandler
-- type instance Result (PureHandler a) = a
-- type instance Inner (PureHandler a) = a

-- handlePure :: Comp (PureHandler a) a -> a
-- handlePure comp = handle comp (\x _ -> x) PureHandler

-- data IOHandler a = IOHandler
-- type instance Result (IOHandler a) = IO a
-- type instance Inner (IOHandler a) = a

-- handleIO :: Comp (IOHandler a) a -> IO a
-- handleIO comp = handle comp (\x _ -> return x) IOHandler 

