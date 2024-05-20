{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    ConstraintKinds,
    DataKinds #-}

module Web where

import HIA.Handlers
import HIA.TopLevel
import HIA.DesugarHandlers
import System.IO
import Control.Monad
import Network
import Control.Concurrent

type RequestType = String
type URL = String

-- [operation|Collect h a :: Comp h a -> a|]
-- reifyHandler :: Comp h h
--[operation|AcceptConnection a :: Servlet a -> ()|]
--[operation|TerminateHeader a :: ServletBody a -> a|]

[operation|EmitHeader      :: String -> String -> ()|]
[operation|Emit            :: String ->           ()|]

[operation|GetRequestType  :: RequestType|]
[operation|GetURL          :: URL|]
[operation|GetValueOf      :: String -> Maybe String|]

type HeaderConstraints  h = ([handles|h {EmitHeader}|]) -- , [handles|h {TerminateHeader}|])
type BodyConstraints    h = ([handles|h {Emit}|])
--type RequestConstraints h = ([handles|h {GetRequestType}|], [handles|h {GetURL}|], [handles|h {GetValueOf}|])

-- type Servlet a = (HeaderConstraints h, BodyConstraints h, RequestConstraints h) => Comp h a
-- type ServletBody a = (BodyConstraints h, RequestConstraints h) => Comp h a
type Servlet a = (HeaderConstraints h, BodyConstraints h) => Comp h a
--type ServletBody a = (BodyConstraints h) => Comp h a

type ServletHandler = (HeaderConstraints h, BodyConstraints h) => h

-- server :: ([handles|h {AcceptConnection a}|]) => Servlet a -> Comp h ()
-- server p = 
--   do acceptConnection p;
--      server p

server :: PortNumber -> (Handle -> Comp h a ->  IO ()) -> Comp h a -> IO ()
server p h servlet = withSocketsDo $ do
    sock <- listenOn $ PortNumber p
    loop sock
    where
      loop sock = do
        (ha,_,_) <- accept sock
        forkIO $ (h ha servlet)
        loop sock
        -- where
        --   body h = do
        --     hPutStr h msg
        --     hFlush h
        --     hClose h
         
main =  
  server 5002 (\ha servlet -> bufferStringOutput [] [] ha servlet) pong



-- parsing HTTP headers
[operation|GetC :: Char|]
[operation|PeekC :: Char|]
[operation|forall a.Failure :: a|]

getUntil :: ([handles|h {GetC}|]) => Char -> Comp h String
getUntil c =
  do c' <- getC
     if c == c'
       then return []
       else do s <- getUntil c
               return (c' : s)

skipSpace :: ([handles|h {PeekC}|], [handles|h {GetC}|]) => Comp h ()
skipSpace = do c <- peekC
               if c == ' '
                 then do getC; skipSpace
                 else return ()

nextLine :: ([handles|h {Failure}|], [handles|h {GetC}|]) => Comp h ()
nextLine = do c <- getC
              if c == '\n'
                then return ()
                else failure

getHeader :: ([handles|h {Failure}|], [handles|h {GetC}|], [handles|h {PeekC}|]) => Comp h (Maybe (String, String))
getHeader = do r <- peekC
               if r == '\r'
                 then do getC; nextLine; return Nothing
                 else
                   do
                     key <- getUntil ':'
                     skipSpace
                     value <- getUntil '\r'
                     nextLine
                     return (Just (key, value))

getFirstLine :: ([handles|h {Failure}|], [handles|h {GetC}|], [handles|h {PeekC}|]) => Comp h (String, String, String)
getFirstLine = do method <- getUntil ' '
                  path <- getUntil ' '
                  version <- getUntil '\r'
                  nextLine
                  return (method, path, version)

-- handling peek in terms of get
[handler|
  forward h handles {GetC}.
    HandlePeek a :: Maybe Char -> a
      handles {PeekC, GetC} where
        Return x   _        -> return x
        GetC     k Nothing  -> do c <- getC; k c Nothing
        GetC     k (Just c) -> k c Nothing
        PeekC    k Nothing  -> do c <- getC; k c (Just c)
        PeekC    k (Just c) -> k c (Just c)
|]
handlePeek' comp = handlePeek Nothing comp

-- A simple handler for Web Application Output
-- Not efficient and cannot handle any header.

[handler| 
     DirectOutput a :: Handle -> IO a
     handles {Emit} where                 
        Return x s -> do {hFlush s; hClose s; return x} 
        Emit c k s -> do {hPutStr s c; k () s}
|]


headerOk = "HTTP/1.0 200 OK\r\n"
-- Build a list of string and of headers
-- not very efficient.
[handler| 
      BufferStringOutput a :: [String] -> [(String,String)]-> Handle -> IO a
      handles {Emit, EmitHeader} where
         Return x ss hs h ->  
                   do
                   hPutStr h headerOk
                   forM_  hs 
                         (\ (k,v) -> do 
                                     hPutStr h k
                                     hPutStr h ": "
                                     hPutStr h v
                                     hPutStr h "\r\n")
                   hPutStr h "\r\n" 
                   forM_  (reverse ss) (\s -> hPutStr h s) 
                   hPutStr h "\r\n"
                   hFlush h;
                   hClose h;
                   return x
         Emit s k ss hs h -> k () (s:ss) hs h
         EmitHeader key val k ss hs h ->
                k () ss ((key,val):hs) h
|]

pong :: ([handles|h {Emit}|]) => Comp h ()
pong = emit "Pong!"


-- a very basic webserver
--
-- main = withSocketsDo $ do
--     sock <- listenOn $ PortNumber 5002
--     loop sock
 
-- loop sock = do
--    (h,_,_) <- accept sock
--    forkIO $ body h
--    loop sock
--   where
--    body h = do
--        hPutStr h msg
--        hFlush h
--        hClose h
 
-- msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
