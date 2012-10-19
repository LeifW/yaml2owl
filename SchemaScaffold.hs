{-# LANGUAGE OverloadedStrings #-}

module SchemaScaffold  where

import Data.List( partition, elem )
import Text.XML.Light
import Data.Text (Text, splitOn, unpack, toLower)
import qualified Data.Text.IO as TIO
import Data.Yaml (decodeFile)
--import Data.HashMap.Strict (HashMap)
import Data.Map (Map, toList)

import Network.URI

import Swish.RDF
--import Swish.Utils.Namespace (ScopedName, getNamespaceTuple, getScopeLocal)
import Swish.Namespace (ScopedName, getNamespaceTuple, getScopeLocal)
--import Swish.Utils.LookupMap (listLookupMap)
import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Vocabulary.OWL
import Swish.RDF.Formatter.Turtle

--import Swish.RDF.RDFQuery -- (rdfFindValSubj, rdfFindPredVal)
import Swish.RDF.Query -- (rdfFindValSubj, rdfFindPredVal)

import System.Directory
import System.FilePath( (</>), (<.>) )

import Data.Maybe (fromJust)


import Yaml2Schema
-- triple Subj Type Class

-- putStrLn $ ppTopElement $ unode "html" (Attr (unqual "href") "google.com", unode "div" ())

baseDir = "app"
mkDir = createDirectoryIfMissing True


instancesOf = rdfFindValSubj resRdfType
classesOf = instancesOf resRdfsClass

xmlns prefix = Attr $ QName prefix Nothing (Just "xmlns")

data Subject = Subject {
  name :: ScopedName,
  dataProps :: [(String, String, String)],
  objectProps :: [(String, String)]
} deriving (Show, Eq)

data DataProperty = DataProperty 

b = putStrLn $ ppElement $ unode "html" [xmlns "foaf" "http://foo.com"]

prefixesOf g = [ xmlns (unpack p) (show u) | (Just p, u) <- map getNamespaceTuple $ listLookupMap $ namespaces g ]

classNames g = [ getScopeLocal sn | Res sn <- classesOf g ]

classInfos g = map classInfo $ classesOf g
  where
    classInfo klass = Subject (getScopedName klass) dataProps' objectProps'
      where
        props = rdfFindValSubj resRdfsDomain klass g
        graphDataProps = rdfFindValSubj resRdfType (Res owlDatatypeProperty) g
        (dataProperties, objectProperties) = partition (\p-> elem p graphDataProps) props
        dataProps' = [ (show p, unpack $ localName p, show $ head $ rdfFindPredVal p resRdfsRange g) | p <- dataProperties ]
        objectProps' = [ (show p, unpack $ localName p)  | p <- objectProperties ]
          
      
--    classInfo klass = Subject (unpack $ toLower $ localName klass) dataProps' objectProps'

--localName (Res r) = getScopeLocal r
localName = getScopeLocal . getScopedName

{-    
classInfos g = do
  klass <- classesOf g
  return klass
-}

layout :: [Attr] -> [Element] -> Element
layout prefixes content =
  unode "html" (prefixes, [
    unode "head" (),
    unode "body" content
  ])

index :: Subject -> [Element]
index subject =
  [
    unode "div" ([about ("[" ++ show (name subject) ++ "]"), rev "rdf:type"], individual subject)
  ]

individual :: Subject -> [Element]
individual (Subject name dataProps objectProps) =
  [
    unode "div" [
      unode "p" [
        unode "b" (l ++ ":"),
        unode "span" [property p, datatype t] 
      ]
    | (p, l, t) <- dataProps ],
    unode "div" [
      unode "p" ([rel p], [
        Text $ blank_cdata {cdData = (l ++ ":")},
        Elem $ unode "a" (href "_", unode "span" (property "rdfs:label"))
      ])
    | (p, l) <- objectProps ]
  ]
  --map (unode "span" . property . fst) dataProps)

attr name = Attr (unqual name)
rel = attr "rel"
rev = attr "rev"
href = attr "href"
about = attr "about"
resource = attr "resource"
property = attr "property"
datatype = attr "datatype"

         -- 
--getGraph = fmap (json2graph . fromJust) $ parseYaml "schema.yml" 

label :: Subject -> String
label = unpack . toLower . getScopeLocal . name

scaffold :: [Attr] -> Subject -> IO ()
scaffold prefixes subject = do
  let dir = baseDir </> label subject
  mkDir dir
  writeFile (dir </> "index" <.> "html") $ ppElement $ layout prefixes $ index subject
  writeFile (dir </> "_wildcard" <.> "html") $ ppElement $ layout prefixes $ individual subject

--  Just json <- parseYaml "schema.yml"
main = do
  json <- decodeFile "schema.yml"
  let g = maybe (error "Invalid YAML structure.") json2graph json
  TIO.writeFile "schema.ttl" $ formatGraphAsText g
  let prefixes = prefixesOf g
  let subjects = classInfos g
  mapM_ (scaffold prefixes) subjects
  putStrLn $ ppElement $ page g
