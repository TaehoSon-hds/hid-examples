{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerUtils (genServer, serveRPC, runSerialized) where

import Data.Serialize ( decode, encode, Serialize )
import Data.ByteString (ByteString)
import Control.Monad ( forever )
import Control.Monad.Catch ( Exception(displayException), MonadCatch(catch) )
import Network.Simple.TCP ( HostName, serve, HostPreference(Host) )
import Network.Connection ( connectFromSocket, initConnectionContext, ConnectionParams(ConnectionParams) )
import Network.Socket (PortNumber)

import RemoteIO ( unEitherStaged, runRemoteConn, sendRSIO, receiveRSIO, throwRemote )
import RpcCommon ( DecodeStages(Stage2), RPCTable, RemoteAction, RSIO, RemoteState, RemoteException )
import DeclsGenerator (genServer)

runSerialized :: (Serialize a, Serialize b) =>
                 RemoteAction st a b ->
                 RemoteAction st ByteString ByteString
runSerialized action params
  = unEitherStaged Stage2 (decode params) >>= fmap encode . action

serveRPC :: RemoteState st =>
          HostName -> PortNumber -> RPCTable st -> IO ()
serveRPC host portNum rpcTable = serve (Host host) (show portNum) procRequests
  where
    connParams = ConnectionParams host portNum Nothing Nothing

    procRequests (connSock, sockAddr) = do
      logConnection "New connection" sockAddr
      initCtx <- initConnectionContext
      conn <- connectFromSocket initCtx connSock connParams
      catch (runRemoteConn conn $ forever $ serveRequest rpcTable)
            (\(e :: RemoteException) ->
               logConnection (displayException e) sockAddr)

    logConnection msg sockAddr =
      putStrLn $ "LOG: " <> show sockAddr <> " " <> msg

serveRequest :: RPCTable st -> RSIO st ()
serveRequest rpcTable = receiveRSIO >>= call >>= sendRSIO
  where
    call (operation, params) =
      case lookup operation rpcTable of
        Nothing -> throwRemote
                   $ "Unsupported operation (" <> operation <> ")"
        Just func -> func params
