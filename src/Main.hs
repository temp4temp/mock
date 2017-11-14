{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Conduit
import qualified Data.Conduit.Combinators as CC

import Control.Monad.IO.Class
import Control.Concurrent
import Data.Conduit.Network

import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as B

echo :: (MonadIO m) => Conduit B.ByteString m B.ByteString
echo = CC.mapM ( \ a -> do (liftIO (print a)); return ("this is echo " <> a))

delay :: (MonadIO m) => Conduit a m a
delay = CC.mapM ( \ a -> do (liftIO (threadDelay 10000)); return a)

netloop :: AppData -> IO ()
netloop appdata = do
  appSource appdata .| echo $$ appSink appdata
  
startMock :: Int -> IO ()
startMock port = do
  runTCPServer (serverSettings port "*") netloop
  return ()
  
pulsloop :: AppData -> IO ()
pulsloop appdata = do
  CC.yieldMany (repeat "light") .| delay $$ appSink appdata
  

startPuls :: IO ()
startPuls = do
  runTCPServer (serverSettings 4567 "*") pulsloop
  return ()

main :: IO ()
main = do
  args <- getArgs
  putStrLn "hello world"
  case null args of
    True -> do startMock 3333
    False -> if (head args) == "mock" 
                then do startMock (read (args!!1))
                else do startMock (read (args!!1))



