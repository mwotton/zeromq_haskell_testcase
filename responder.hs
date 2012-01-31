{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Control.Monad
import System.IO
import System.Console.CmdArgs
import qualified Data.Text as T

import qualified System.ZMQ as ZMQ
-- import qualified Data.ByteString as SB

data SocketArgs = SocketArgs {endpoint :: String} deriving (Show, Data, Typeable)

getArgs = cmdArgsMode SocketArgs
    { endpoint = "inproc://local"  &= argPos 0 &= typ "0MQ endpoint" }
    
sendF flags s msg = ZMQ.send s msg flags

main :: IO ()
main = do
    sock_args <- cmdArgsRun getArgs
    ZMQ.withContext 1 $ \c ->
      ZMQ.withSocket c ZMQ.Rep $ \s -> do
        ZMQ.bind s $ endpoint sock_args
        forever $ ZMQ.poll [ZMQ.S s ZMQ.In] 1000000 >>= mapM_ receive
 where
    receive (ZMQ.S s e) = do 
      msg <- ZMQ.receive s [] 
      ZMQ.send s msg []


