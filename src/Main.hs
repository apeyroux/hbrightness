{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Scientific (coefficient
                                 , toRealFloat)
import qualified Data.Text as T
import           HBrightness.Opt
import           HBrightness.Types
import           Shelly

default (T.Text)

pMonitor :: T.Text -> Parser Monitor
pMonitor name = Monitor
  <$> pure name
  <*> (manyTill' anyChar (string name) *> space *> ((== "connected") <$> many' letter))
  <*> (space *> ((== "primary") <$> many' letter))
  <*> (toRealFloat <$> (manyTill' anyChar (string "Brightness:") *> space *> scientific))

xrandr :: IO T.Text
xrandr = shelly $ silently $ run "xrandr" ["--verbose"]

upBrightness :: Monitor -> IO ()
upBrightness Monitor { mBrightness = 1.0 } = return ()
upBrightness m = shelly $ silently $ run_ "xrandr" ["--output"
                                                   , mName m
                                                   , "--brightness"
                                                   , T.pack $ show (mBrightness m + 0.1)]

downBrightness :: Monitor -> IO ()
downBrightness Monitor { mBrightness = 0.3 } = return ()
downBrightness m = shelly $ silently $ run_ "xrandr" ["--output"
                                                   , mName m
                                                   , "--brightness"
                                                   , T.pack $ show (mBrightness m - 0.1)]

main :: IO ()
main = do
  -- todo faire un check si xrandr est installé
  args <- getOpts
  xr <- xrandr
  case args of
    Opt { optAction = Up } -> 
      case maybeResult $ parse (pMonitor "eDP-1") xr of
        Just m -> upBrightness m
        Nothing -> putStrLn "Monitor not found"
    Opt { optAction = Down } -> 
      case maybeResult $ parse (pMonitor "eDP-1") xr of
        Just m -> downBrightness m
        Nothing -> putStrLn "Monitor not found"
