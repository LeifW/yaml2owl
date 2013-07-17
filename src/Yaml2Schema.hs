{-# LANGUAGE OverloadedStrings #-}

module Yaml2Schema ( json2graph ) where

import Prelude hiding ((/))
import Data.Text (Text, splitOn, unpack)
import Data.Yaml (decodeFile)
import Data.Map (Map, toList)

import Network.URI (URI(..), URIAuth(..))

import Data.Set (fromList)

import Swish.RDF
import Swish.Namespace (ScopedName, makeScopedName)
import Swish.QName (newLName)
import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Vocabulary.OWL
import Swish.RDF.Formatter.Turtle

type TextMap = Map Text
type MapMap = TextMap (TextMap Text)

url :: String -> String -> URI
url domain path = URI "http:" (Just $ URIAuth "" domain "") ('/':path) "" ""

domain / path = url domain path

ex :: Text -> ScopedName
ex lname = maybe (error $ "Invalid chars in local name: " ++ unpack lname) (makeScopedName (Just "ex") ("example.org"/"scheme#")) $ newLName lname

resolve :: Text -> ScopedName
resolve n = case splitOn ":" n of
  [name] -> ex name
  [prefix, name] -> error "Not implemented yet"

--(a, b) --> c =  a + b - c
(-->)
  :: (ToRDFLabel s, ToRDFLabel p, ToRDFLabel o) =>
     (s, p) -> o -> RDFTriple
(a, b) --> c = toRDFTriple a b c

json2graph :: MapMap -> RDFGraph
json2graph = toRDFGraph . fromList . json2triples

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
