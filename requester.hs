{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Control.Monad
import System.IO
import System.Console.CmdArgs
import qualified Data.Text as T
import Control.Concurrent
import Control.Exception(finally)
import System.IO.Unsafe(unsafePerformIO)
import qualified System.ZMQ as ZMQ
import qualified Data.ByteString.Char8 as BS

data SocketArgs = SocketArgs {reps:: Int, endpoint :: String} deriving (Show, Data, Typeable)

getArgs = cmdArgsMode SocketArgs
    { endpoint = "inproc://local"  &= argPos 0 &= typ "0MQ endpoint", 
      reps = 100  &= argPos 1 &= typ "reps"
    }
    


children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])
    
waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
        mvar <- newEmptyMVar
        childs <- takeMVar children
        putMVar children (mvar:childs)
        forkIO (io `finally` putMVar mvar ())


main :: IO ()
main = do
    sock_args <- cmdArgsRun getArgs
    let e = endpoint sock_args
    let r = reps sock_args
    ZMQ.withContext 1 $ \c -> do
      forM_ [1..100] $ \i -> forkChild $ do
        putStrLn $ show i
        ZMQ.withSocket c ZMQ.Req $ \s -> do
          ZMQ.connect s e
          forM_ [1..r] $ \j -> do
            ZMQ.send s (BS.pack $ show (i,j)) []
            msg <- ZMQ.receive s []
            return ()
          putStrLn ("Thread " ++ show i ++ " sent " ++ show r ++ " messages.")
    putStrLn "Waiting for children in one thread, hopefully"
    waitForChildren
