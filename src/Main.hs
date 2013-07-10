{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml (decodeFile)
import qualified Data.Text.IO as TIO

import Swish.RDF.Formatter.Turtle (formatGraphAsText)

import Yaml2Schema (json2graph)
import SchemaScaffold (prefixesOf, classInfos, scaffold)

baseDir :: String
baseDir = "app"

main :: IO ()
main = do
  json <- decodeFile "schema.yml"
  let g = maybe (error "Invalid YAML structure.") json2graph json
  TIO.writeFile "schema.ttl" $ formatGraphAsText g
  let prefixes = prefixesOf g
  let subjects = classInfos g
  mapM_ (scaffold baseDir prefixes) subjects
