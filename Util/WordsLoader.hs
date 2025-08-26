{-# OPTIONS_GHC -Wall #-}

module Util.WordsLoader (loadWordsFile) where

import Control.Exception (IOException, try)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)

loadWordsFile :: FilePath -> Q Exp
loadWordsFile path = do
    addDependentFile path
    either (fail . ("Could not read words file: " <>) . show) (lift . T.lines)
        =<< runIO (try @IOException $ T.readFile path)
