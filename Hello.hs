{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -threaded #-}

module Hello (main) where

import Data.Time (getCurrentTime)

main :: IO ()
main = do
    print =<< getCurrentTime
    putStrLn "yo"
