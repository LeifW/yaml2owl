{-# LANGUAGE OverloadedStrings #-}

module Yaml2Schema ( parseYaml, json2graph ) where

import Prelude hiding ((/))
import Data.List (nub)
import Data.Text (Text, splitOn, unpack)
import Data.Yaml (decodeFile)
--import Data.HashMap.Strict (HashMap)
import Data.Map (Map, toList)

import Network.URI

import Swish.RDF
import Swish.Utils.Namespace (ScopedName, makeScopedName)
import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Vocabulary.OWL
import Swish.RDF.TurtleFormatter

type TextMap = Map Text
type MapMap = TextMap (TextMap Text)

url :: String -> String -> URI
url domain path = URI "http:" (Just $ URIAuth "" domain "") ('/':path) "" ""

domain / path = url domain path

ex = makeScopedName (Just "ex") ("example.org"/"scheme#")

resolve :: Text -> ScopedName
resolve n = case splitOn ":" n of
  [name] -> ex name
  [prefix, name] -> error "Not implemented yet"

--(a, b) --> c =  a + b - c
(-->)
  :: (ToRDFLabel s, ToRDFLabel p, ToRDFLabel o) =>
     (s, p) -> o -> RDFTriple
(a, b) --> c = toRDFTriple a b c

parseYaml :: FilePath -> IO (Maybe MapMap)
parseYaml = decodeFile

json2graph :: MapMap -> RDFGraph
json2graph = toRDFGraph . nub . json2triples

json2triples :: MapMap -> [RDFTriple]
json2triples m = do
  (subj, assertions) <- toList m
  let klass = Res (ex subj)
  (Arc klass resRdfType resRdfsClass) : concat (map (props klass) (toList assertions))

props ::RDFLabel -> (Text, Text)-> [RDFTriple]
props domain (p, r) = [
    toRDFTriple prop resRdfType typ,  
    toRDFTriple prop resRdfsDomain domain,  
    toRDFTriple prop resRdfsRange range
  ]
    where
      prop = resolve p
      (range, typ) = case r of
        "String" -> (xsdString, owlDatatypeProperty)
        "Int" -> (xsdInteger, owlDatatypeProperty)
        other -> (resolve other, owlObjectProperty)
        
{--
do
  (subject, assertions) <- toList undefined
  (property, value) <- assertions
  return subject
--}
-- triple Subj Type Class

main = do
  m <- decodeFile "schema.yml"::IO (Maybe MapMap)
  let g = maybe (error "Invalid yaml") (toRDFGraph . nub . json2triples) m
  print g
  putStrLn ""
  putStrLn $ unpack $ formatGraphAsText g

-- putStrLn $ ppTopElement $ unode "html" (Attr (unqual "href") "google.com", unode "div" ())
