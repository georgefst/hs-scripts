{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module ImageMetaFileName (main) where

import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding
import Options.Generic
import RawFilePath
import System.IO
import System.Posix.ByteString
import Text.Pretty.Simple
import Util.Error qualified as Error

{-
Rename image files to match the date they were taken (in the format used by android).
This is actually idempotent (even without the `filter` for "IMG_" prefix), which is nice.
Note that the `identify` subprocess (from ImageMagick) is really slow.
-}

main :: IO ()
main = do
    changeWorkingDirectory "/home/gthomas/Pictures/synced/unsorted"
    (filter alreadyDone <$> listDirectory ".") >>= traverse_ \f -> do
        let convertDate = \case
                [y1, y2, y3, y4, ':', m1, m2, ':', d1, d2, ' ', h1, h2, ':', min1, min2, ':', s1, s2] ->
                    [y1, y2, y3, y4, m1, m2, d1, d2, '_', h1, h2, min1, min2, s1, s2]
                x -> err "unexpected format" (f, x)
        (exitCode, outBS, errBS) <- readProcessWithExitCode $ proc "identify" ["-verbose", f]
        pHPrint stderr (f, exitCode, errBS)
        rename f
            . encodeUtf8
            . ("IMG_" <>)
            . (<> ".jpg")
            . T.pack
            . convertDate
            . T.unpack
            . fromMaybe (err "no date found" f)
            . firstJust (T.stripPrefix "exif:DateTime: ")
            . map T.strip
            . T.lines
            . decodeUtf8
            $ outBS

alreadyDone :: RawFilePath -> Bool
alreadyDone = not . ("IMG_" `T.isPrefixOf`) . decodeUtf8

progName :: Text
progName = "image-meta-file-name"

err :: Show a => Text -> a -> b
err = Error.err progName
