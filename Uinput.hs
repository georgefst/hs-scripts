{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Uinput where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Evdev.Codes qualified as Codes

import Evdev

main :: IO ()
main = do
    d <- newUDevice $ ps4opts "5"
    forever $ do
        threadDelay 500_000
        ev d Codes.BtnSouth

new :: ByteString -> IO UDevice
new = newUDevice . ps4opts

enumerate :: (Enum a, Bounded a) => [a]
enumerate = enumFromTo minBound maxBound

ev :: UDevice -> Codes.Key -> IO ()
ev dev k =
    writeBatch dev [KeyEvent k Pressed]
        >> threadDelay 50000
        >> writeBatch dev [KeyEvent k Released]

-- only matches keys and axes (note DS4 also has FF events)
ps4opts :: ByteString -> NewUDevice
ps4opts name =
    NewUDevice
        { name = name,
          uniq = Nothing,
          phys = Nothing,
          idProduct = Nothing,
          idVendor = Nothing,
          idBustype = Nothing,
          idVersion = Nothing,
          keys =
              [ Codes.BtnTl,
                Codes.BtnTr,
                Codes.BtnTl2,
                Codes.BtnTr2,
                Codes.BtnSelect,
                Codes.BtnStart,
                Codes.BtnMode,
                Codes.BtnThumbl,
                Codes.BtnThumbr,
                Codes.BtnSouth,
                Codes.BtnEast,
                Codes.BtnNorth,
                Codes.BtnWest
              ],
          absAxes =
              zip
                  [ Codes.AbsX,
                    Codes.AbsY,
                    Codes.AbsZ,
                    Codes.AbsRx,
                    Codes.AbsRy,
                    Codes.AbsRz,
                    Codes.AbsHat0x,
                    Codes.AbsHat0y
                  ]
                  $ repeat
                  $ AbsInfo
                      { absValue = 127,
                        absMinimum = 0,
                        absMaximum = 255,
                        absFuzz = 0,
                        absFlat = 0,
                        absResolution = 0
                      },
          relAxes = [],
          miscs = [Codes.MscScan],
          switchs = [],
          leds = [],
          sounds = []
        }
