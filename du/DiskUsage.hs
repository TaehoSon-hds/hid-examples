module DiskUsage (diskUsage) where

import App
    ( liftM2,
      when,
      MonadReader(ask),
      modify,
      MonadState(get),
      MonadWriter(tell),
      FileOffset,
      fileSize,
      isDirectory,
      isRegularFile,
      AppEnv(AppEnv, cfg, fileStatus, depth, path),
      AppConfig(maxDepth),
      MyApp )
import Utils ( traverseDirectoryWith, currentPathStatus, checkExtension )

data DUEntryAction =
    TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fs
      | isDirectory fs =
            TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path =
            RecordFileSize (fileSize fs)
      | otherwise = None

    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+fsize)
    processEntry None = pure ()
