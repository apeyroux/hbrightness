{-# LANGUAGE OverloadedStrings #-}

module HBrightness.Types (
  Action (..)
  , Opt (..)
  , Monitor (..)
  ) where

import qualified Data.Text as T

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
