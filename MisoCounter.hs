{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | This is basically a "hello world" for Miso, to check that the build script logic for Wasm modules is working etc.
module MisoCounter (main) where

import Miso
import Miso.String

main :: IO ()
main =
    run
        . startApp
        $ defaultApp
            (0 :: Word)
            (\() n -> pure $ succ n)
            (\n -> div_ [] [button_ [onClick ()] [text "+"], text $ ms n])
            ()

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
