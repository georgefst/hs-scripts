{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Util where

import Data.Text (Text)
import Data.Text.IO qualified as T

modifyFile :: (Text -> Text) -> FilePath -> IO ()
modifyFile f file = T.writeFile file . f =<< T.readFile file
