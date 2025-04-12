{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | This is basically a "hello world" for Miso, to check that the build script logic for Wasm modules is working etc.
module MisoCounter (main) where

import Control.Monad.State
import Miso
import Miso.String

main :: IO ()
main = run $ startApp app

app :: App Effect Word () ()
app =
    defaultApp
        0
        (\() -> modify succ)
        (\n -> div_ [] [button_ [onClick ()] [span_ [] [text "+"]], text $ ms n])

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
