{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Lib
import System.Environment
import Control.Applicative
import Data.Text (Text)
import Data.Semigroup((<>))
import Options.Applicative

import qualified Data.Text              as Text
import qualified Options.Applicative
import qualified System.IO              as IO


version :: Text
version = "0.1.0"

data Options = Options
  { optOS :: String
  , optOSV :: String }

operatingSystem :: Parser String
operatingSystem = strOption
    (  long "os"
    <> metavar "OPERATING_SYSTEM"
    <> help "Operating System to Target" )

operatingSystemVersion :: Parser String
operatingSystemVersion = strOption
    (  long "osv"
    <> metavar "OS_VERSION"
    <> help "Operating System Version to Target"
    )

opts :: ParserInfo Options
opts = info((Options <$> operatingSystem <*> operatingSystemVersion) <**> helper)
  (  fullDesc
  <> progDesc "Check Ansible Playbook"
  <> header "ansiblecheck - a testing framework for ansible")

main :: IO()
main = do
  options <- execParser opts
  putStrLn $ writeOS options


writeOS :: Options -> String
writeOS (Options os osv) = "Operating System: " ++ os ++ " -- Operating System Version: " ++ osv
