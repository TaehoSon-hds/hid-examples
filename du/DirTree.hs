module DirTree where

import App
    ( when,
      MonadReader(ask),
      MonadWriter(tell),
      takeBaseName,
      isDirectory,
      AppEnv(AppEnv, cfg, fileStatus, depth, path),
      AppConfig(maxDepth),
      MyApp )
import Utils ( traverseDirectoryWith, currentPathStatus )

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
      tell [(takeBaseName path, depth)]
      traverseDirectoryWith dirTree
