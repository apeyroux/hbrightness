{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Scientific (coefficient
                                 , toRealFloat)
import qualified Data.Text as T
import qualified Options.Applicative as OA
import           Shelly

default (T.Text)

data Action = Up | Down deriving (Show, Read, Eq)

data Opt = Opt {
  optScreenName :: String
  , optAction :: Action
  } deriving Show

data Monitor = Monitor {
  mName :: T.Text
  , mConnected :: Bool
  , mPrimary :: Bool
  , mBrightness :: Double
  } deriving Show

newtype Brightness = Brightness Double deriving (Show, Read, Eq)

instance Bounded Brightness where
  minBound = Brightness 0.1
  maxBound = Brightness 1.0

pOpt :: OA.Parser Opt
pOpt = Opt
  <$> OA.strOption
    (OA.long "monitor"
     <> OA.short 'm'
     <> OA.metavar "MONITORNAME")
  <*> OA.option OA.auto
    (OA.long "action"
     <> OA.short 'a'
     <> OA.value Up
     <> OA.metavar "Up|Down")-- (pure Up OA.<|> pure Down)

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
  args <- OA.execParser (OA.info pOpt OA.fullDesc)
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
