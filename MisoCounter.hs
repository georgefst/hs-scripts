{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | This is basically a "hello world" for Miso, to check that the build script logic for Wasm modules is working etc.
module MisoCounter (main) where

import Miso
import Miso.String

main :: IO ()
main = do
#ifdef wasi_HOST_OS
    let styles = []
#else
    styles <- pure @[] . Style . ms <$> readFile "web/miso-counter.css"
#endif
    run $ startComponent app{styles}

app :: Component Word ()
app =
    component
        0
        (\() -> modify succ)
        (\n -> div_ [] [button_ [onClick ()] [span_ [] [text "+"]], text $ ms n])

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
