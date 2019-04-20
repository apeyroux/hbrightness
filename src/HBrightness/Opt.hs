{-# LANGUAGE OverloadedStrings #-}

module HBrightness.Opt (getOpts) where

import qualified Data.Text as T
import           HBrightness.Types
import           Options.Applicative

pOpt :: Parser Opt
pOpt = Opt
  <$> strOption
    (long "monitor"
     <> short 'm'
     <> metavar "MONITORNAME")
  <*> option auto
    (long "action"
     <> short 'a'
     <> value Up
     <> metavar "Up|Down")-- (pure Up OA.<|> pure Down)

getOpts :: IO Opt
getOpts = execParser (info pOpt fullDesc)
