{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module CabScriptToEnv where

-- Assumes formatting like:
{- cabal:
build-depends:
    base >= 4.14,
    process ^>= 1.6.10,
-}

--TODO don't ignore versions
--TODO use cabal-env

import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main =
    getArgs >>= \case
        [file] -> do
            libs <- getLibs file
            callProcess "cabal" $ ["install", "--package-env", ".", "--lib"] ++ libs
        _ -> error "need input file arg"

-- >>> getLibs "Build.hs"
getLibs :: FilePath -> IO [String]
getLibs file = do
    in' <- lines <$> readFile file
    return $ map (head . words) . takeWhile ((== ' ') . head) $ tail $ dropWhile (/= "build-depends:") in'
