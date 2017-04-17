module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype OperatingSystem = OperatingSystem {name :: String}
                     deriving (Show)
