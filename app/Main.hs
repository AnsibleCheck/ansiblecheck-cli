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
  print (writeOS $ fromOptions options)


writeOS :: OperatingSystem -> String
writeOS os = "Operating System: " ++ show os

data Ubuntu = Yakkety | Xenial | Trusty | Precise
  deriving (Eq, Show, Read, Ord)

data EL     = EL7 | EL6
  deriving (Eq, Show, Read, Ord)
data OEL    = OEL7 | OEL6
  deriving (Eq, Show, Read, Ord)

data OperatingSystem = Ubuntu Ubuntu
                     | EL EL
                     | OEL OEL
  deriving (Eq, Show, Read, Ord)

data ServiceManager = SystemD | Init
  deriving (Eq, Show, Read, Ord)

osServiceManager :: OperatingSystem -> ServiceManager
osServiceManager (Ubuntu Yakkety) = Init
osServiceManager (Ubuntu Precise) = Init
osServiceManager (EL EL6)         = Init
osServiceManager (OEL OEL6)       = Init
osServiceManager a                = SystemD


fromOptions :: Options -> OperatingSystem
fromOptions (Options os osv) = read (os ++ " " ++ osv)