{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Scientific (coefficient
                                 , toRealFloat)
import qualified Data.Text as T
import           Shelly

default (T.Text)

data Action = Up | Down deriving Show

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
downBrightness Monitor { mBrightness = 0.1 } = return ()
downBrightness m = shelly $ silently $ run_ "xrandr" ["--output"
                                                   , mName m
                                                   , "--brightness"
                                                   , T.pack $ show (mBrightness m - 0.1)]

main :: IO ()
main = do
  putStrLn "hbrightness"
  putStrLn "plop"
