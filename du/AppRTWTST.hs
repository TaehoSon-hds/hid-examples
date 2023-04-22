{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module AppRTWTST where

import Control.Monad.Writer ( MonadIO, WriterT(runWriterT), MonadWriter )
import Control.Monad.Reader ( MonadReader, ReaderT(..) )
import Control.Monad.State ( StateT, MonadState, evalStateT )

import AppTypes ( AppEnv, AppConfig, initialEnv )

newtype MyApp logEntry state a = MyApp {
      runApp :: ReaderT AppEnv
                  (WriterT [logEntry]
                      (StateT state
                              IO)) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppEnv,
                MonadWriter [logEntry],
                MonadState state)

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st =
  evalStateT
         (runWriterT
               (runReaderT (runApp app) (initialEnv config)))
         st
