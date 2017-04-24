{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text)
import Lib
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.Process
import System.Console.ANSI
import Text.Read

import qualified Data.Text as Text
import qualified Options.Applicative
import qualified System.IO as IO

version :: Text
version = "0.1.0"

data Options = Options
  { optOS :: String
  , optOSV :: String
--  , optDeps :: [String]
  }

operatingSystem :: Parser String
operatingSystem =
  strOption (long "os" <> metavar "OPERATING_SYSTEM" <> help "Operating System to Target")

operatingSystemVersion :: Parser String
operatingSystemVersion =
  strOption (long "osv" <> metavar "OS_VERSION" <> help "Operating System Version to Target")

--dependencies :: Parser [String]
--dependencies =
--  listOption (str >>= parseStringList)
--         ( short 'd' <> long "dependency" <> metavar "DEPENDENCY" <> help "Ansible Galaxy Dependency to Add" )

opts :: ParserInfo Options
opts =
  info
    ((Options <$> operatingSystem <*> operatingSystemVersion) <**> helper)
    (fullDesc <> progDesc "Check Ansible Playbook" <>
     header "ansiblecheck - a testing framework for ansible")

main :: IO ()
main = do
  options <- execParser opts
  fp <- System.Directory.getCurrentDirectory
  run fp $ fromOptions options

run :: FilePath -> Maybe OperatingSystem -> IO ()
run fp (Just os) = do
  putStrLn "> Current Working Directory:"
  putStrLn fp
  (_, Just hout, Just herr, _) <-
    createProcess (pullDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  pullOut <- IO.hGetContents hout
  pullErr <- IO.hGetContents herr
  putStrLn "> Pull"
  putStrLn "> StdOut"
  putStrLn pullOut
  putStrLn "> StdErr"
  putStrLn pullErr
  (_, Just hout, Just herr, _) <-
    createProcess (runDocker fp os) {std_out = CreatePipe, std_err = CreatePipe}
  createOut <- IO.hGetContents hout
  createErr <- IO.hGetContents herr
  putStrLn "> Create"
  putStrLn "> StdOut"
  putStrLn createOut
  putStrLn "> StdErr"
  putStrLn createErr
  (_, Just hout, Just herr, _) <-
    createProcess (syntaxCheckDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  syntaxOut <- IO.hGetContents hout
  syntaxErr <- IO.hGetContents herr
  putStrLn "> Syntax Check"
  putStrLn "> StdOut"
  putStrLn syntaxOut
  putStrLn "> StdErr"
  putStrLn syntaxErr
  (_, Just hout, Just herr, _) <-
    createProcess (testDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  testOut <- IO.hGetContents hout
  testErr <- IO.hGetContents herr
  putStrLn "> Tests"
  putStrLn "> StdOut"
  putStrLn testOut
  putStrLn "> StdErr"
  putStrLn testErr
  (_, Just hout, Just herr, _) <-
    createProcess (testDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  idempotenceOut <- IO.hGetContents hout
  idempotenceErr <- IO.hGetContents herr
  putStrLn "> Idempotence"
  putStrLn "> StdOut"
  putStrLn idempotenceOut
  putStrLn "> StdErr"
  putStrLn idempotenceErr
  writeIdempotence idempotenceOut
  (_, Just hout, _, _) <- createProcess (stopDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  (_, Just hout, _, _) <-
    createProcess (removeDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  return ()
run _ Nothing = putStrLn "Invalid Options Input"


writeIdempotence :: String -> IO()
writeIdempotence str = do
  _ <- if isIdempotent
        then
          setSGR [SetColor Foreground Vivid Green]
        else
          setSGR [SetColor Foreground Vivid Red]
  _ <- if isIdempotent
        then
          putStrLn "> Idempotent: True"
        else
          putStrLn "> Idempotent: False"
  _ <- setSGR[Reset]
  return ()
  where
    isIdempotent = Text.isInfixOf "changed=0" (Text.pack str) && Text.isInfixOf "failed=0" (Text.pack str)


writeOS :: OperatingSystem -> (String, String)
writeOS (Ubuntu Yakkety) = ("ubuntu", "yakkety")
writeOS (Ubuntu Xenial) = ("ubuntu", "xenial")
writeOS (Ubuntu Trusty) = ("ubuntu", "trusty")
writeOS (Ubuntu Precise) = ("ubuntu", "precise")
writeOS (EL EL7) = ("el", "7")
writeOS (EL EL6) = ("el", "6")
writeOS (OEL OEL7) = ("oel", "7")
writeOS (OEL OEL6) = ("oel", "6")

writeInit :: OperatingSystem -> String
writeInit (Ubuntu Yakkety) = "/lib/systemd/systemd"
writeInit (Ubuntu Xenial) = "/lib/systemd/systemd"
writeInit (Ubuntu Trusty) = "/sbin/init"
writeInit (Ubuntu Precise) = "/sbin/init"
writeInit (EL EL7) = "/lib/systemd/systemd"
writeInit (EL EL6) = "/sbin/init"
writeInit (OEL OEL7) = "/lib/systemd/systemd"
writeInit (OEL OEL6) = "/sbin/init"

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

installDependencyDocker :: OperatingSystem -> String -> CreateProcess
installDependencyDocker operatingS dependency =
  System.Process.shell
    (
     "docker" ++
     " " ++
     "exec" ++
     " " ++
     "--tty" ++
     " " ++
     containerN ++
     " " ++
     "env TERM=xterm" ++
     " " ++
     "ansible-galaxy install " ++ dependency
   )
  where
    containerN = containerName operatingS "tests" "test.yml"


pullDocker :: OperatingSystem -> CreateProcess
pullDocker operatingS =
  System.Process.proc "docker" ["pull", "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv]
  where
    (os, osv) = writeOS operatingS

runDocker :: FilePath -> OperatingSystem -> CreateProcess
runDocker fp operatingS =
  System.Process.shell
    ("docker" ++
     " " ++
     "run" ++
     " " ++
     "--detach" ++
     " " ++
     "--name=" ++
     containerN ++
     " " ++
     "--volume=" ++
     show fp ++
     ":" ++
     "/etc/ansible/roles/role_under_test:ro" ++
     " " ++
     "--volume=/sys/fs/cgroup:/sys/fs/cgroup:ro" ++
     " " ++ "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv ++ " " ++ initS)
  where
    (os, osv) = writeOS operatingS
    containerN = containerName operatingS "tests" "test.yml"
    initS = writeInit operatingS

syntaxCheckDocker :: OperatingSystem -> CreateProcess
syntaxCheckDocker operatingS =
  System.Process.shell
    ("docker" ++
     " " ++
     "exec" ++
     " " ++
     "--tty" ++
     " " ++
     containerN ++
     " " ++
     "env TERM=xterm" ++
     " " ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml --syntax-check")
  where
    containerN = containerName operatingS "tests" "test.yml"

testDocker :: OperatingSystem -> CreateProcess
testDocker operatingS =
  System.Process.shell
    ("docker" ++
     " " ++
     "exec" ++
     " " ++
     "--tty" ++
     " " ++
     containerN ++
     " " ++
     "env TERM=xterm" ++ " " ++ "ansible-playbook /etc/ansible/roles/role_under_test/tests/test.yml")
  where
    containerN = containerName operatingS "tests" "test.yml"

stopDocker :: OperatingSystem -> CreateProcess
stopDocker operatingS = System.Process.proc "docker" ["stop", containerN]
  where
    containerN = containerName operatingS "tests" "test.yml"

removeDocker :: OperatingSystem -> CreateProcess
removeDocker operatingS = System.Process.proc "docker" ["rm", "--force", containerN]
  where
    containerN = containerName operatingS "tests" "test.yml"

containerName :: OperatingSystem -> FilePath -> FilePath -> String
containerName operatingS projectFile testFile = "ansiblecheck-" ++ os ++ "-" ++ osv
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
