{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( replicateM_, void )
import Control.Monad.Trans ( MonadIO(liftIO) )

import ClientUtils ( runRemote, remote, callRemote )
import PingCommon ( PingAnswer, RemotePing )

[remote|
ping :: RemotePing PingAnswer
echo :: String -> RemotePing String
 |]

example :: Int -> RemotePing ()
example n = do
    echo "Hello from client" >>= prt
    replicateM_ n (ping >>= prt)
    echo "Bye from client" >>= prt
  where
    prt :: Show a => a -> RemotePing ()
    prt = liftIO . print

main :: IO ()
main = void $ runRemote "localhost" 1500 (example 3)
