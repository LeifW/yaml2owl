{-# LANGUAGE OverloadedStrings #-}

module SchemaScaffold (prefixesOf, classInfos, scaffold) where

import Data.List (partition)
import Text.XML.Light
import Data.Text (unpack, toLower)
import Data.Map (toList)

import Swish.RDF
import Swish.Namespace (ScopedName, getScopeLocal)
import Swish.QName (getLName, LName)
import Swish.RDF.Vocabulary.OWL

import Swish.RDF.Query (rdfFindValSubj, rdfFindPredVal)

import System.Directory
import System.FilePath( (</>), (<.>) )

mkDir :: FilePath -> IO ()
mkDir = createDirectoryIfMissing True

instancesOf :: RDFLabel -> RDFGraph -> [RDFLabel]
instancesOf = rdfFindValSubj resRdfType

classesOf :: RDFGraph -> [RDFLabel]
classesOf = instancesOf resRdfsClass

xmlns :: String -> String -> Attr
xmlns prefix = Attr $ QName prefix Nothing (Just "xmlns")

data Subject = Subject {
  name :: ScopedName,
  dataProps :: [(String, String, String)],
  objectProps :: [(String, String)]
} deriving (Show, Eq)

prefixesOf :: NSGraph lb -> [Attr]
prefixesOf g = [ xmlns (unpack p) (show u) | (Just p, u) <- toList $ namespaces g ]

--classNames g = [ getScopeLocal sn | Res sn <- classesOf g ]

classInfos :: NSGraph RDFLabel -> [Subject]
classInfos g = map classInfo $ classesOf g
  where
    classInfo klass = Subject (getScopedName klass) dataProps' objectProps'
      where
        props = rdfFindValSubj resRdfsDomain klass g
        graphDataProps = rdfFindValSubj resRdfType (Res owlDatatypeProperty) g
        (dataProperties, objectProperties) = partition (\p-> elem p graphDataProps) props
        dataProps' = [ (show p, (unpack . getLName . localName) p, show $ head $ rdfFindPredVal p resRdfsRange g) | p <- dataProperties ]
        objectProps' = [ (show p, (unpack . getLName . localName) p)  | p <- objectProperties ]
      
localName :: RDFLabel -> LName
localName = getScopeLocal . getScopedName

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

attr :: String -> String -> Attr
attr name = Attr (unqual name)

rel, rev, href, about, resource, property, datatype :: String -> Attr
rel = attr "rel"
rev = attr "rev"
href = attr "href"
about = attr "about"
resource = attr "resource"
property = attr "property"
datatype = attr "datatype"

label :: Subject -> String
label = unpack . toLower . getLName . getScopeLocal . name

scaffold :: String -> [Attr] -> Subject -> IO ()
scaffold baseDir prefixes subject = do
  let dir = baseDir </> label subject
  mkDir dir
  writeFile (dir </> "index" <.> "html") $ ppElement $ layout prefixes $ index subject
  writeFile (dir </> "_wildcard" <.> "html") $ ppElement $ layout prefixes $ individual subject
