{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RemoteIO where

import RpcCommon ( DecodeStages(Stage1, Stage0), RSIO(runRem), RemoteState(..), RemoteException(RemoteException, ConnectionClosed), msgSizeField )
import Control.Monad.Reader ( MonadIO(liftIO), MonadReader(ask), ReaderT(runReaderT) )
import Control.Monad.State ( evalStateT )
import Control.Monad.Catch ( Exception(displayException), MonadCatch(catch), MonadThrow(throwM) )
import Network.Connection ( connectTo, connectionClose, connectionGetExact, connectionPut, initConnectionContext, Connection, ConnectionParams(ConnectionParams) )
import Network.Socket (PortNumber)
import System.IO.Error (isEOFError)


import Data.Serialize ( decode, encode, getWord64be, runGet, putByteString, putWord64be, runPut, Serialize )
import qualified Data.ByteString as BS

unEitherStaged :: DecodeStages -> Either String a -> RSIO st a
unEitherStaged stage = either (throwRemote . errMsg) pure
  where
    errMsg msg = "Decoding error (" <> show stage <> "): " <> msg

runRemote :: RemoteState st => String -> PortNumber -> RSIO st a -> IO a
runRemote host port computation = do
    conn <- remoteConnectTo host port
    res <- runRemoteConn conn computation
    liftIO $ connectionClose conn
    pure res

runRemoteConn :: RemoteState st => Connection -> RSIO st a -> IO a
runRemoteConn conn computation =
    runReaderT (evalStateT (runRem computation) initState) conn

sendRSIO :: Serialize a => a -> RSIO st ()
sendRSIO msg = do
    conn <- ask
    liftIO $ connectionPut conn $ buildMsgEnvelope $ encode msg
  where
    buildMsgEnvelope payload = runPut $ do
      putWord64be (fromIntegral $ BS.length payload)
      putByteString payload

receiveRSIO :: Serialize a => RSIO st a
receiveRSIO = ask >>= \conn ->
          recvExact conn msgSizeField
          >>= unEitherStaged Stage0 . runGet getWord64be
          >>= recvExact conn . fromIntegral
          >>= unEitherStaged Stage1 . decode
  where
    recvExact conn sz =
      catch (liftIO $ connectionGetExact conn sz)
            (\e -> if isEOFError e then throwM ConnectionClosed
                   else throwRemote (displayException e))

throwRemote :: String -> RSIO st b
throwRemote err_msg = throwM $ RemoteException err_msg

remoteConnectTo :: String -> PortNumber -> IO Connection
remoteConnectTo host port = do
    connCtx <- initConnectionContext
    connectTo connCtx connParams
  where
    connParams = ConnectionParams host port Nothing Nothing
