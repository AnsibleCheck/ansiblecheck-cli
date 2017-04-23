{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text)
import Lib
import Options.Applicative
import System.Environment
import Text.Read
import System.Directory
import System.Exit
import System.Process

import qualified Data.Text as Text
import qualified Options.Applicative
import qualified System.IO as IO

version :: Text
version = "0.1.0"

data Options = Options
  { optOS :: String
  , optOSV :: String
  }

operatingSystem :: Parser String
operatingSystem =
  strOption (long "os" <> metavar "OPERATING_SYSTEM" <> help "Operating System to Target")

operatingSystemVersion :: Parser String
operatingSystemVersion =
  strOption (long "osv" <> metavar "OS_VERSION" <> help "Operating System Version to Target")

opts :: ParserInfo Options
opts =
  info
    ((Options <$> operatingSystem <*> operatingSystemVersion) <**> helper)
    (fullDesc <> progDesc "Check Ansible Playbook" <>
     header "ansiblecheck - a testing framework for ansible")

--main :: IO ()
--main = do
--  options <- execParser opts
--  print (writeOS $ fromOptions options)

main :: IO ()
main = do
  fp <- System.Directory.getCurrentDirectory
  runXenial fp


runXenial :: FilePath -> IO()
runXenial fp = do
  _ <- pullDocker (Ubuntu Xenial)
  _ <- runDocker fp (Ubuntu Xenial)
  _ <- syntaxCheckDocker (Ubuntu Xenial)
  _ <- testDocker (Ubuntu Xenial)
  _ <- stopDocker (Ubuntu Xenial)
  _ <- removeDocker (Ubuntu Xenial)
  return ()


writeOS :: Maybe OperatingSystem -> String
writeOS mOS =
  case mOS of
    Just os -> "Operating System: " ++ show os
    Nothing -> "Operating System: Failed to parse"

data Ubuntu
  = Yakkety
  | Xenial
  | Trusty
  | Precise
  deriving (Eq, Show, Read, Ord)

data EL
  = EL7
  | EL6
  deriving (Eq, Show, Read, Ord)

data OEL
  = OEL7
  | OEL6
  deriving (Eq, Show, Read, Ord)

data OperatingSystem
  = Ubuntu Ubuntu
  | EL EL
  | OEL OEL
  deriving (Eq, Show, Read, Ord)

data ServiceManager
  = SystemD
  | Init
  deriving (Eq, Show, Read, Ord)


pullDocker :: OperatingSystem -> IO ExitCode
pullDocker _ =
  System.Process.system ("docker pull ansiblecheck/ansiblecheck:" ++ "ubuntu" ++ "-" ++ "xenial")

runDocker :: FilePath -> OperatingSystem -> IO ExitCode
runDocker fp _ =
  System.Process.system (
  "docker run --detach" ++
  " " ++
  "--name=" ++ "ansiblecheck-ubuntu-xenial" ++
  " " ++
  "--volume=" ++ show fp ++ ":/etc/ansible/roles/role_under_test:ro" ++
  " " ++
  "--volume=/sys/fs/cgroup:/sys/fs/cgroup:ro" ++
  " " ++
  "ansiblecheck/ansiblecheck:" ++ "ubuntu" ++ "-" ++ "xenial" ++
  " " ++
  "/lib/systemd/systemd"
  )

syntaxCheckDocker :: OperatingSystem -> IO ExitCode
syntaxCheckDocker _ =
  System.Process.system (
  "docker exec --tty"
  ++ " "
  ++ "ansiblecheck-ubuntu-xenial"
  ++ " "
  ++ "env TERM=xterm"
  ++ " "
  ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml --syntax-check"
  )

testDocker :: OperatingSystem -> IO ExitCode
testDocker _ =
  System.Process.system (
  "docker exec"
  ++ " "
  ++ "ansiblecheck-ubuntu-xenial"
  ++ " "
  ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml"
  )

stopDocker :: OperatingSystem -> IO ExitCode
stopDocker _ =
  System.Process.system "docker stop ansiblecheck-ubuntu-xenial"

removeDocker :: OperatingSystem -> IO ExitCode
removeDocker _ =
  System.Process.system "docker rm ansiblecheck-ubuntu-xenial"

osServiceManager :: OperatingSystem -> ServiceManager
osServiceManager (Ubuntu Yakkety) = Init
osServiceManager (Ubuntu Precise) = Init
osServiceManager (EL EL6) = Init
osServiceManager (OEL OEL6) = Init
osServiceManager _ = SystemD

fromOptions :: Options -> Maybe OperatingSystem
fromOptions (Options os osv) = readMaybe (os ++ " " ++ osv)

