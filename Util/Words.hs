{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Words (allWords) where

import Data.Text (Text)
import Util.WordsLoader

allWords :: [Text]
allWords = $(loadWordsFile "/usr/share/dict/words")
