{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Conduit
import qualified Data.Conduit.Combinators as CC

import Control.Monad.IO.Class
import Control.Concurrent
import Data.Conduit.Network

import qualified Data.ByteString.Char8 as B

echo :: (MonadIO m) => Conduit B.ByteString m B.ByteString
echo = CC.mapM ( \ a -> do (liftIO (print a)); return ("this is echo " <> a))

netloop :: AppData -> IO ()
netloop appdata = do
  appSource appdata .| prn $$ appSink appdata
  
startMock :: IO ()
startMock = do
  runTCPServer (serverSettings 4567 "*") netloop
  return ()

main :: IO ()
main = do
  putStrLn "hello world"
  startMock


