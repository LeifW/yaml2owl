{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Data.Yaml (decodeFileEither)
import qualified Data.Text.IO as TIO

import Swish.RDF.Formatter.Turtle (formatGraphAsText)

import Yaml2Schema (json2graph)
import SchemaScaffold (prefixesOf, classInfos, scaffold)

baseDir :: String
baseDir = "app"

printUsage :: IO ()
printUsage = putStrLn "Usage: yaml2owl schema.ttl"

run :: [String] -> IO ()
run [yamlFile] = do
  json <- decodeFileEither yamlFile
  let g = either (error . show) json2graph json
  TIO.writeFile "schema.ttl" $ formatGraphAsText g
  let prefixes = prefixesOf g
  let subjects = classInfos g
  mapM_ (scaffold baseDir prefixes) subjects
run _ = printUsage


main :: IO ()
main = do
  args <- getArgs
  run args
