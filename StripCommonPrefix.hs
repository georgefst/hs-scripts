{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{- | Remove a common prefix from the names of all files in the given directory.
Developed for some stems I was sent by Jacob as it makes it easier to see the track names in Ardour.
-}
module StripCommonPrefix where

import Control.Monad (zipWithM_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Text (Text, commonPrefixes)
import Data.Text qualified as T
import System.Directory (listDirectory, renameFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main =
    getArgs >>= \case
        [dir] -> do
            fs <- map T.pack <$> listDirectory dir
            case commonPrefixes' fs of
                Nothing -> putStrLn "no common prefix found!" >> exitFailure
                Just (p, fs') -> do
                    putStrLn $ "common prefix found: " <> show p
                    confirm >>= \case
                        False -> exitFailure
                        True -> zipWithM_ (renameFile `on` ((dir </>) . T.unpack)) fs fs'
            pure ()
        _ -> error "bad args"

-- TODO move to Util module
{- Util -}

-- TODO take custom message as arg?
confirm :: IO Bool
confirm =
    -- TODO use monad-loops?
    confirmOnce >>= \case
        Just x -> pure x
        Nothing -> confirm
confirmOnce :: IO (Maybe Bool)
confirmOnce = do
    putStrLn "Proceed? [y/n]"
    getChar <&> \case
        'y' -> Just True
        'n' -> Just False
        _ -> Nothing

-- TODO better name
-- TODO QuickCheck
commonPrefixes' ::
    [Text] ->
    Maybe
        ( Text -- prefix
        , [Text] -- remainders
        )
commonPrefixes' = \case
    [] -> Nothing -- TODO what should this really be? maybe take a NonEmpty
    [x] -> Just (x, [""])
    x : xs -> do
        (p, r) <- commonPrefixes' xs
        (q, x', p') <- commonPrefixes x p
        pure (q, x' : map (p' <>) r)
