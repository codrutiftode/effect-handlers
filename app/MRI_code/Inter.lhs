> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}

> module MRI_code.Inter where

> import Control.Monad.Writer
> import MRI_code.Advice hiding (fib2)
> import MRI_code.Effects (fib2)

> logfib = new (log <@> fib2)
>   where 
>    log :: MonadWriter String m => Open (Int -> m Int)
>    log super n = do  tell "entering fib"
>                      super n
>
