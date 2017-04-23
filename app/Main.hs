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

main :: IO ()
main = do
  options <- execParser opts
  fp      <- System.Directory.getCurrentDirectory
  run fp $ fromOptions options

run :: FilePath -> Maybe OperatingSystem -> IO()
run fp (Just os) = do
  _ <- pullDocker os
  _ <- runDocker fp os
  _ <- syntaxCheckDocker os
  _ <- testDocker os
  _ <- stopDocker os
  _ <- removeDocker os
  return ()
run _ Nothing =
  putStrLn "Invalid Options Input"


writeOS :: OperatingSystem -> (String, String)
writeOS (Ubuntu Yakkety)  = ("ubuntu", "yakkety")
writeOS (Ubuntu Xenial)   = ("ubuntu", "xenial")
writeOS (Ubuntu Trusty)   = ("ubuntu", "trusty")
writeOS (Ubuntu Precise)  = ("ubuntu", "precise")
writeOS (EL EL7)          = ("el", "7")
writeOS (EL EL6)          = ("el", "6")
writeOS (OEL OEL7)        = ("oel", "7")
writeOS (OEL OEL6)        = ("oel", "6")

writeInit :: OperatingSystem -> String
writeInit (Ubuntu Yakkety)  = "/lib/systemd/systemd"
writeInit (Ubuntu Xenial)   = "/lib/systemd/systemd"
writeInit (Ubuntu Trusty)   = "/sbin/init"
writeInit (Ubuntu Precise)  = "/sbin/init"
writeInit (EL EL7)          = "/lib/systemd/systemd"
writeInit (EL EL6)          = "/sbin/init"
writeInit (OEL OEL7)        = "/lib/systemd/systemd"
writeInit (OEL OEL6)        = "/sbin/init"

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
pullDocker operatingS =
  System.Process.system (
  "docker"
  ++ " "
  ++ "pull"
  ++ " "
  ++ "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv
  )
  where
    (os, osv) = writeOS operatingS


runDocker :: FilePath -> OperatingSystem -> IO ExitCode
runDocker fp operatingS =
  System.Process.system (
  "docker"
  ++ " "
  ++ "run"
  ++ " "
  ++ "--detach"
  ++ " "
  ++ "--name=" ++ "ansiblecheck-" ++ os ++ "-" ++ osv
  ++ " "
  ++ "--volume=" ++ show fp ++ ":" ++ "/etc/ansible/roles/role_under_test:ro"
  ++ " "
  ++ "--volume=/sys/fs/cgroup:/sys/fs/cgroup:ro"
  ++ " "
  ++ "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv
  ++ " "
  ++ initS
  )
  where
      (os, osv) = writeOS operatingS
      initS     = writeInit operatingS

syntaxCheckDocker :: OperatingSystem -> IO ExitCode
syntaxCheckDocker operatingS =
  System.Process.system (
  "docker"
  ++ " "
  ++ "exec"
  ++ " "
  ++ "--tty"
  ++ " "
  ++ "ansiblecheck-" ++ os ++ "-" ++ osv
  ++ " "
  ++ "env TERM=xterm"
  ++ " "
  ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml --syntax-check"
  )
  where
      (os, osv) = writeOS operatingS

testDocker :: OperatingSystem -> IO ExitCode
testDocker operatingS =
  System.Process.system (
  "docker"
  ++ " "
  ++ "exec"
  ++ " "
  ++ "ansiblecheck-" ++ os ++ "-" ++ osv
  ++ " "
  ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml"
  )
  where
      (os, osv) = writeOS operatingS

stopDocker :: OperatingSystem -> IO ExitCode
stopDocker operatingS =
  System.Process.system (
  "docker"
   ++ " "
   ++ "stop"
   ++ " "
   ++ "ansiblecheck-" ++ os ++ "-" ++ osv
  )
  where
      (os, osv) = writeOS operatingS

removeDocker :: OperatingSystem -> IO ExitCode
removeDocker operatingS =
  System.Process.system (
  "docker"
  ++ " "
  ++ "rm"
  ++ " "
  ++ "ansiblecheck-" ++ os ++ "-" ++ osv
  )
  where
      (os, osv) = writeOS operatingS

osServiceManager :: OperatingSystem -> ServiceManager
osServiceManager (Ubuntu Yakkety) = Init
osServiceManager (Ubuntu Precise) = Init
osServiceManager (EL EL6) = Init
osServiceManager (OEL OEL6) = Init
osServiceManager _ = SystemD

fromOptions :: Options -> Maybe OperatingSystem
fromOptions (Options os osv) = readMaybe (os ++ " " ++ osv)

