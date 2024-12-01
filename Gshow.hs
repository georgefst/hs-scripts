{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Gshow (main) where

import Data.Data
import Data.Generics.Aliases (extQ)
import Data.Generics.Schemes (everywhere)
import Data.Text (Text)

-- gshowMy :: (Data a) => a -> String
-- gshowMy x = gshowsMy x "" []

gshowsWith :: (Data a, Typeable b) => (b -> ShowS) -> a -> ShowS
gshowsWith f =
    ( \t ->
        showChar '('
            . (showString . showConstr . toConstr $ t)
            . (foldr (.) id . gmapQ ((showChar ' ' .) . gshowsWith f) $ t)
            . showChar ')'
    )
        `extQ` (shows :: String -> ShowS)
        `extQ` f

gshowWith :: (Data a, Typeable b) => (b -> ShowS) -> a -> String
gshowWith f x = gshowsWith f x ""

-- gshow' = gshow

customGShow :: (Data a, Typeable b) => (b -> String) -> a -> String
customGShow f =
    ( \t ->
        "("
            ++ show (toConstr t)
            ++ concatMap (' ' :) (gmapQ (customGShow f) t)
            ++ ")"
    )
        `extQ` show @String
        `extQ` f

-- Usage example
data Example = Example Int Text | Rec () Example deriving (Data, Typeable)
example :: Example
example = Rec () $ Example 42 "Hello"

main :: IO ()
main = do
    putStrLn $ customGShow (\(i :: Int) -> "<int: " ++ show i ++ ">") example
    putStrLn $ gshowWith (\(i :: Int) _s -> "<int: " ++ show i ++ ">") example

-- everything from HLS splice plugin GHC 9.10 debugging
-- showsModSimple :: ParsedSource -> ShowS
-- showsModSimple =
--     gshowsWith
--         ( \(s :: SrcSpan) ->
--             -- ("george-src-span-placeholder" <>)
--             shows s
--         )
-- showsMod :: (Data a) => a -> ShowS
-- showsMod =
--     ( \t ->
--         showChar '('
--             . (showString . showConstr . toConstr $ t)
--             . (foldr (.) id . gmapQ ((showChar ' ' .) . showsMod) $ t)
--             . showChar ')'
--     )
--         `extQ` (shows :: String -> ShowS)
--         `extQ` (\(s :: SrcSpan) -> shows s)
--         `extQ` (\(s :: FastString) -> shows s)

-- gshowsWith :: (Data a, Typeable b) => (b -> ShowS) -> a -> ShowS
-- gshowsWith f =
--     ( \t ->
--         showChar '('
--             . (showString . showConstr . toConstr $ t)
--             . (foldr (.) id . gmapQ ((showChar ' ' .) . gshowsWith f) $ t)
--             . showChar ')'
--     )
--         `extQ` (shows :: String -> ShowS)
--         `extQ` f

-- pPrint :: (Show a) => a -> IO ()
-- pPrint = putStrLn <=< readProcess "pretty-simple" [] . show
-- {-# NOINLINE pShow #-}
-- pShow :: String -> String
-- pShow = unsafePerformIO . readProcess "pretty-simple" []
-- {-# NOINLINE pShowNoColor #-}
-- pShowNoColor :: String -> String
-- pShowNoColor = unsafePerformIO . readProcess "pretty-simple" ["-cno-color"]
-- {-# NOINLINE traceFile #-}
-- traceFile :: String -> String -> a -> a
-- traceFile fp s x = unsafePerformIO $ writeFile fp s >> pure x
