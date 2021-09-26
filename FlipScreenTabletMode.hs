{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -threaded #-}

module FlipScreenTabletMode (main) where

import Evdev
import Evdev.Codes
import Evdev.Stream
import Streamly.Prelude qualified as S
import System.Process

main :: IO ()
main = do
    dev <- newDevice "/dev/input/event21"
    S.mapM_ (f . eventData) (readEvents dev)

--TODO get below working reliably and/or allow CLI arg for device path
--TODO filter to devices which can emit the relevant event
-- main = S.mapM_ f $ eventData . snd <$> readEvents allDevices

f :: EventData -> IO ()
f = \case
    SwitchEvent SwTabletMode (EventValue 0) -> do
        putStrLn "normal"
        -- https://www.faqforge.com/linux/rotating-screen-in-ubuntu-and-linux-mint/
        callProcess "xrandr" ["-o", "normal"]
        -- https://unix.stackexchange.com/questions/39959/reversing-the-direction-of-a-trackball
        -- https://wiki.ubuntu.com/X/InputCoordinateTransformation
        callProcess "xinput" ["--set-prop", "ELAN902C:00 04F3:2DCF", "175", "1", "0", "0", "0", "1", "0", "0", "0", "1"]
    SwitchEvent SwTabletMode (EventValue 1) -> do
        putStrLn "inverted"
        callProcess "xrandr" ["-o", "inverted"]
        callProcess "xinput" ["--set-prop", "ELAN902C:00 04F3:2DCF", "175", "-1", "0", "1", "0", "-1", "1", "0", "0", "1"]
    _ -> pure ()
