{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Error where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe qualified as Unsafe
import Text.Pretty.Simple (pShow)

{-# NOINLINE err #-}
err :: Show a => Text -> Text -> a -> b
err prog s x = Unsafe.unsafePerformIO do
    tmp <- getTemporaryDirectory
    time <- getCurrentTime
    let logDir = tmp </> "hs-script-logs"
        logFile = logDir </> T.unpack prog <.> "log"
    createDirectoryIfMissing False logDir
    T.appendFile logFile $ T.unlines [T.pack $ show time, s, TL.toStrict $ pShow x, ""]
    error $ "Failed: see log at: " <> logFile
