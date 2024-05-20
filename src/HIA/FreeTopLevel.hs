{- Free top-level handlers -}

{-# LANGUAGE TypeFamilies,
    MultiParamTypeClasses,
    GADTs,
    TypeOperators,
    RankNTypes,
    FlexibleContexts,
    QuasiQuotes
  #-}

module HIA.FreeTopLevel where

import HIA.FreeHandlers
import HIA.DesugarHandlers

handlePure :: (forall h. Comp h a) -> a
[handler|
  HandlePure a :: a handles {} where
    Return x -> x
|]

handleIO :: Comp (HandleIO a) a -> IO a
[operation|forall a.Io :: IO a -> a|]
[handler|
  HandleIO a :: IO a handles {Io} where
    Return x -> return x
    Io m k   -> do {x <- m; k x}
|]
