{-# LANGUAGE GADTs, TypeFamilies, RankNTypes,
    MultiParamTypeClasses, FlexibleInstances,
    FlexibleContexts, UndecidableInstances, TypeOperators,
    QuasiQuotes
  #-}

module Logic where

import Control.Monad
import Control.Applicative

import HIA.Handlers
import HIA.TopLevel
import HIA.DesugarHandlers


[operation|forall a.Failure ::        a|]
[operation|forall a.Choose  :: [a] -> a|]

infixr :-
data SomeList a = Last a | a :- SomeList a
  deriving Show

[operation|forall a.ChooseSome :: SomeList a -> a|]


type Logic a = [handles|h {Choose}|] => Comp h a

[handler|
  AllResultsPure a :: [a]
    handles {Choose} where
      Return x   -> [x]
      Choose l k -> concatMap k l
|]

-- [handler|
--   AllResultsPure a :: [a]
--     handles {Choose} where
--       Return x   -> [x]
--       Choose l k -> step l []
--                     where
--                       step []     zs = zs
--                       step (x:xs) zs = step xs (k x ++ zs)
-- |]


-- big fat memory leak!
[handler|
  forward h.AllResultsLeaky a :: [a]
    handles {Choose} where
      Return x   -> return [x]
      Choose l k -> liftM concat (mapM k l)
|]


[handler|
  AllResultsSilly h a :: Comp h [a]
    handles {Choose} where
      Return x       -> return [x]
      Choose l     k -> step l []
                        where
                          step []     rs = return rs
                          step (x:xs) rs = do {ys <- k x; step xs (rs ++ ys)}
|]

[handler|
  forward h.AllResults a :: [a]
    handles {Choose} where
      Return x       -> return [x]
      Choose l     k -> step l []
                        where
                          step []     rs = return rs
                          step (x:xs) rs = do {ys <- k x; step xs (rs ++ ys)}
|]

failed :: Logic a
failed = choose []

[handler|
  forward h.MaybeResults a :: Maybe a
    handles {Choose} where
      Return x    -> return (Just x)
      Choose xs k -> pickFirst xs
          where
            pickFirst []     = return Nothing
            pickFirst (x:xs) = do r <- k x
                                  case r of
                                    Just _  -> return r
                                    Nothing -> pickFirst xs
|]

-- contorted way of picking the first result or failing
-- without materialising a maybe type
data Stack h a = Stack ([Stack h a -> Comp h a])
[handler|
  forward h handles {Failure}.FirstHandler a :: Stack h a -> a
    handles {Choose} where
      Return x        _              -> return x
      Choose []     k (Stack [])     -> failure
      Choose []     k (Stack (x:xs)) -> x (Stack xs)
      Choose (a:as) k (Stack l)      -> k a (Stack (map k as ++ l))
|]
firstResult :: ((h `Handles` Failure) ()) => Logic a -> Comp h a
firstResult comp = firstHandler (Stack []) comp

-- an alternative way of avoiding materialising a maybe type
[operation|Success a :: a -> ()|]
[handler|
  forward h handles {Success a}.SuccessFailure a :: () handles {Choose} where
    Return x    -> success x
    Choose xs k -> foldl (\c x -> c >> k x) (return ()) xs
|]
successOrFailure :: ([handles|h {Success a}|]) => Logic a -> Comp h ()
successOrFailure comp = successFailure comp

[handler|
  forward h.
    MaybeSuccess a :: Maybe a handles {Success a} where
      Return x    -> return Nothing
      Success v k -> return (Just v)
|]

[handler|
   MaybeSuccessRaw a :: Maybe a handles {Success a} where
     Return x    -> Nothing
     Success v k -> Just v
|]


[handler|
  IterativeHandler a :: Int -> (Bool, [a])
    handles {Choose} where
      Return x   i -> if i == 0 then (False, [x]) else (False, [])
      Choose l k i -> if i == 0 then (True, [])
                      else
                        let (bs, xss) = unzip (map (\x -> k x $! i-1) l) in
                        (any id bs, concat xss)
|]
iterativeResults :: Logic a -> [[a]]
iterativeResults comp =
  foldr
    (\(b, xs) xss -> xs:(if b then xss else []))
    []
   (map (\i -> iterativeHandler i comp) [0..])

test1 :: Logic [Int]
test1 =
  do
    i <- choose [1..10]
    j <- choose [1..10]
    if (i+j) `mod` 2 == 0 then failed
      else return [i, j]

safeAddition :: [Int] -> Int -> Int -> Bool
safeAddition [] _ _ = True
safeAddition (r:rows) row i =
   row /= r &&
   abs (row - r) /= i &&
   safeAddition rows row (i + 1)

queens :: Int -> [[Int]]
queens n = foldM f [] [1..n] where
    f rows _ = [row : rows |
                row <- [1..n],
                safeAddition rows row 1]

queens0 :: Int -> [[Int]]
queens0 n = foldM f [] [1..n] where
    f rows _ = do row <- [1..n]
                  if (safeAddition rows row 1)
                    then [row : rows]
                    else []

check b = if b then return ()
          else failed

-- queens' :: Int -> Logic [Int]
-- queens' n = foldM f [] [1..n] where
--     f rows _ = do row <- choose [1..n]
--                   check (safeAddition rows row 1)
--                   return (row : rows)

queens' :: Int -> Logic [Int]
queens' n = foldM f [] [1..n] where
    f rows _ = do row <- choose [1..n]
                  if (safeAddition rows row 1)
                    then return (row : rows)
                    else failed

queens'' :: Int -> Logic [Int]
queens'' n = foldM f [] [1..n] where
    f rows _ = do row <- choose [1..n]
                  if (safeAddition rows row 1)
                    then return (row : rows)
                    else failed


test2 n = (handlePure (maybeResults (queens' n)))
test3 n = (handlePure ((maybeSuccess . successFailure)  (queens' n)))
test4 n = head (queens n)
test5 n = (maybeSuccessRaw . successFailure)  (queens' n)

test6 n = handlePure (allResults (queens' n))
test7 n = queens n
test8 n = allResultsPure (queens' n)
test9 n = handlePure (allResultsSilly (queens' n))
test10 n = handlePure (allResultsLeaky (queens' n))



main = print (maximum (test10 13))
