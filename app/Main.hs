{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
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

type Dependencies = [String]
type Dependency = String
type ActiveBool = Bool

version :: Text
version = "0.1.0"

data Options = Options
  { optOS :: String
  , optOSV :: String
  , optDeps:: [String]
  }

main :: IO ()
main = do
  options <- execParser opts
  fp <- System.Directory.getCurrentDirectory
  tests <- System.Directory.listDirectory $ fp ++ "/tests/"
  run fp tests (osFromOptions options) (depsFromOptions options)
  where
    osFromOptions :: Options -> Maybe OperatingSystem
    osFromOptions (Options os osv _) = readMaybe (os ++ " " ++ osv)

    depsFromOptions :: Options -> Dependencies
    depsFromOptions (Options _ _ deps) = deps

    opts :: ParserInfo Options
    opts =
      info
        ((Options <$> operatingSystemP <*> operatingSystemVersionP <*> many dependencyP) <**> helper)
        (fullDesc <> progDesc "Check Ansible Playbook" <>
         header "ansiblecheck - a testing framework for ansible")
      where
        operatingSystemP :: Parser String
        operatingSystemP =
          strOption (  short 'o'
                    <> long "os"
                    <> metavar "OPERATING_SYSTEM"
                    <> help "Operating System to Target")

        operatingSystemVersionP :: Parser String
        operatingSystemVersionP =
          strOption (  short 's'
                    <> long "osv"
                    <> metavar "OS_VERSION"
                    <> help "Operating System Version to Target")
        dependencyP :: Parser String
        dependencyP =
          strOption (  short 'd'
                    <> long "dep"
                    <> metavar "DEPENDENCY"
                    <> help "Ansible Galaxy Dependency to Add")

run :: FilePath -> [FilePath] -> Maybe OperatingSystem -> Dependencies ->  IO ()
run fp tests (Just os) deps  = do
  putStrLn $ "> Current Working Directory:" ++ fp
  print tests
  (TestOutput _ _ _ _ _ _ _ _ _ _ idempotenceOut _) <- runOS fp deps os True
  writeIdempotence idempotenceOut
  return ()
run _ _ Nothing _ = putStrLn "Invalid Options Input"

data TestOutput = TestOutput {
  pullOut           :: String
  , pullErr         :: String
  , createOut       :: String
  , createErr       :: String
  , syntaxOut       :: String
  , syntaxErr       :: String
  , depOut          :: String
  , depErr          :: String
  , testOut         :: String
  , testErr         :: String
  , idempotenceOut  :: String
  , idempotenceErr  :: String
} deriving (Show, Read, Eq, Ord)

runOS :: FilePath -> Dependencies -> OperatingSystem -> Bool -> IO TestOutput
runOS cwd deps os active = do
  (_, Just hout, Just herr, ph) <- createProcess (pullDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  pullOut                       <- IO.hGetContents hout
  pullErr                       <- IO.hGetContents herr
  pullExitCode                  <- waitForProcess ph
  (_, Just hout, Just herr, ph) <- createProcess (runDocker cwd os) {std_out = CreatePipe, std_err = CreatePipe}
  createExitCode                <- waitForProcess ph
  createOut                     <- IO.hGetContents hout
  createErr                     <- IO.hGetContents herr
  (depsOut, depsErr)            <- ioInstallDependenciesDocker os deps
  (_, Just hout, Just herr, ph) <- createProcess (syntaxCheckDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  syntaxExitCode                <- waitForProcess ph
  syntaxOut                     <- IO.hGetContents hout
  syntaxErr                     <- IO.hGetContents herr
  (_, Just hout, Just herr, ph) <- createProcess (testDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  testOut                       <- IO.hGetContents hout
  testErr                       <- IO.hGetContents herr
  when active $ putStrLn testOut
  testExitCode                  <- waitForProcess ph
  (_, Just hout, Just herr, ph) <- createProcess (testDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  idempotenceOut                <- IO.hGetContents hout
  idempotenceErr                <- IO.hGetContents herr
  idempotenceExitCode           <- waitForProcess ph
  (_, Just hout, _, _)          <- createProcess (stopDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  (_, Just hout, _, _)          <- createProcess (removeDocker os) {std_out = CreatePipe, std_err = CreatePipe}
  return $ TestOutput pullOut pullErr createOut createErr syntaxOut syntaxErr depsOut depsErr testOut testErr idempotenceOut idempotenceErr


writeIdempotence :: String -> IO()
writeIdempotence str = do
  _ <- if roleOutputIsIdempotent str
        then
          setSGR [SetColor Foreground Dull Green]
        else
          setSGR [SetColor Foreground Dull Red]
  _ <- if roleOutputIsIdempotent str
        then
          putStrLn "> Idempotent: True"
        else
          putStrLn "> Idempotent: False"
  _ <- setSGR[Reset]
  return ()

roleOutputIsIdempotent :: String -> Bool
roleOutputIsIdempotent str = Text.isInfixOf "changed=0" (Text.pack str) && Text.isInfixOf "failed=0" (Text.pack str)


writeOS :: OperatingSystem -> (String, String)
writeOS (Ubuntu Yakkety) = ("ubuntu", "yakkety")
writeOS (Ubuntu Xenial) = ("ubuntu", "xenial")
writeOS (Ubuntu Trusty) = ("ubuntu", "trusty")
writeOS (Ubuntu Precise) = ("ubuntu", "precise")
writeOS (EL EL7) = ("el", "7")
writeOS (EL EL6) = ("el", "6")
writeOS (OEL OEL7) = ("oraclelinux", "7")
writeOS (OEL OEL6) = ("oraclelinux", "6")

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

ioInstallDependenciesDocker :: OperatingSystem -> [String] -> IO (String, String)
ioInstallDependenciesDocker os deps = do
  depT <- traverse (ioInstallDependencyDocker os) deps
  return $ foldl mappend mempty depT
  where
  ioInstallDependencyDocker :: OperatingSystem -> String -> IO (String, String)
  ioInstallDependencyDocker os str = do
    (_, Just hout, Just herr, ph) <- createProcess (installDependencyDocker os str) {std_out = CreatePipe, std_err = CreatePipe}
    phExitCode      <- waitForProcess ph
    depsOut <- IO.hGetContents hout
    depsErr <- IO.hGetContents herr
    return (depsOut, depsErr)

pullDocker :: OperatingSystem -> CreateProcess
pullDocker operatingS =
  System.Process.shell (
    "docker"
    ++ " "
    ++ "pull"
    ++ " "
    ++ "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv
  )
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
     " " ++
     "--privileged" ++
     " " ++
     "ansiblecheck/ansiblecheck:" ++ os ++ "-" ++ osv ++ " " ++ initS)
  where
    (os, osv) = writeOS operatingS
    containerN = containerName operatingS "tests" "test.yml"
    initS = writeInit operatingS

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


