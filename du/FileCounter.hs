module FileCounter (fileCount) where

import System.Directory.Extra (listFiles)

import App
    ( when,
      MonadIO(liftIO),
      MonadReader(ask),
      MonadWriter(tell),
      isDirectory,
      AppEnv(AppEnv, cfg, fileStatus, depth, path),
      AppConfig(maxDepth),
      MyApp )
import Utils ( traverseDirectoryWith, currentPathStatus, checkExtension )

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
      traverseDirectoryWith fileCount
      files <- liftIO $ listFiles path
      tell [(path, length $ filter (checkExtension cfg) files)]
