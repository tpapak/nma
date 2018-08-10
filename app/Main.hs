module Main where

import Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import qualified Data.Map.Strict as Map

import Data.NMA

main :: IO ()
main = do
  putStrLn "running nma"
