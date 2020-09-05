{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Util.Error where

import Data.Text.Lazy qualified as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe qualified as Unsafe
import Text.Pretty.Simple (pShow)

{-# NOINLINE err #-}
err :: Show a => String -> String -> a -> b
err prog s x = Unsafe.unsafePerformIO do
    tmp <- getTemporaryDirectory
    time <- getCurrentTime
    let logDir = tmp </> "hs-script-logs"
        logFile = logDir </> prog <.> "log"
    createDirectoryIfMissing False logDir
    appendFile logFile $ unlines [show time, s, T.unpack $ pShow x, ""]
    error $ "Failed: see log at: " <> logFile
