-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Codec.Epub.Opf.Metadata
   where

import Control.Monad.Error
import Data.Tree.NTree.TypeDefs ( NTree )
import HSH.Command
import Text.Printf
import Text.XML.HXT.Arrow


{- These data types were constructed by studying the IDPF OPF 
   specification  for ePub documents found here:

   http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html
-}

data EMTitle = EMTitle
   (Maybe String) -- xml:lang attribute
   String         -- content
   deriving Show

data EMCreator = EMCreator
   (Maybe String) -- opf:role attribute
   (Maybe String) -- opf:file-as attribute
   String         -- content
   deriving Show

data EMDate = EMDate
   (Maybe String) -- opf:event attribute
   String         -- content
   deriving Show

data Id = Id
   String         -- id attribute
   (Maybe String) -- opf:scheme attribute
   String         -- content
   deriving Show

data EpubMeta = EpubMeta
   { emEMTitles :: [EMTitle]   -- one required
   , emEMCreators :: [EMCreator]
   , emContributors :: [EMCreator]
   , emSubjects :: [String]
   , emDescription :: Maybe String
   , emPublisher :: Maybe String
   , emEMDates :: [EMDate]
   , emType :: Maybe String
   , emFormat :: Maybe String
   , emId :: [Id]          -- one required
   , emSource :: Maybe String
   , emLang :: [String]    -- one required
   , emRelation :: Maybe String
   , emCoverage :: Maybe String
   , emRights :: Maybe String
   }
   deriving Show

-- Note: This isn't valid as-is, some required values are empty lists!
emptyEpubMeta :: EpubMeta
emptyEpubMeta = EpubMeta
   { emEMTitles = []   -- one required
   , emEMCreators = []
   , emContributors = []
   , emSubjects = []
   , emDescription = Nothing
   , emPublisher = Nothing
   , emEMDates = []
   , emType = Nothing
   , emFormat = Nothing
   , emId = []       -- one required
   , emSource = Nothing
   , emLang = []     -- one required
   , emRelation = Nothing
   , emCoverage = Nothing
   , emRights = Nothing
   }


-- HXT helpers

hxtParseParams :: [(String, String)]
hxtParseParams = [(a_validate, v_0)]

atTag :: (ArrowXml a) => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

atQTag :: (ArrowXml a) => QName -> a (NTree XNode) XmlTree
atQTag tag = deep (isElem >>> hasQName tag)

text :: (ArrowXml a) => a (NTree XNode) String
text = getChildren >>> getText

notNullA :: (ArrowList a) => a [b] [b]
notNullA = isA $ not . null

mbGetAttrValue :: (ArrowXml a) =>
   String -> a XmlTree (Maybe String)
mbGetAttrValue n =
   (getAttrValue n >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)

mbGetQAttrValue :: (ArrowXml a) =>
   QName -> a XmlTree (Maybe String)
mbGetQAttrValue qn =
   (getQAttrValue qn >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)


{- ePub parsing helpers

   Note that these URIs could conceivably change in the future
   Is it ok that they're hardcoded like this?
-}

dcName, opfName, xmlName :: String -> QName
dcName local = mkQName "dc" local "http://purl.org/dc/elements/1.1/"
opfName local = mkQName "opf" local "http://www.idpf.org/2007/opf"
xmlName local = mkQName "xml" local "http://FIXME-WRONG"


opfPath :: (MonadError String m, MonadIO m) =>
   FilePath -> m String
opfPath zipPath = do
   containerContents <- extractFileFromZip zipPath
      "META-INF/container.xml"

   result <- liftIO $ runX (
      readString hxtParseParams containerContents
      >>> atTag "rootfile" >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError
         "ERROR: rootfile full-path missing from META-INF/container.xml"


getTitle :: (ArrowXml a) => a (NTree XNode) EMTitle
getTitle = atQTag (dcName "title") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< EMTitle l c


getCreator :: (ArrowXml a) => a (NTree XNode) EMCreator
getCreator = atQTag (dcName "creator") >>>
   proc x -> do
      r <- mbGetQAttrValue (opfName "role") -< x
      f <- mbGetQAttrValue (opfName "file-as") -< x
      c <- text -< x
      returnA -< EMCreator r f c


getDate :: (ArrowXml a) => a (NTree XNode) EMDate
getDate = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< EMDate e c


getMeta :: (ArrowXml a) => a (NTree XNode) EpubMeta
getMeta = atTag "metadata" >>>
   proc x -> do
      ts <- listA getTitle -< x
      cs <- listA getCreator -< x
      ds <- listA getDate -< x
      returnA -< emptyEpubMeta
         { emEMTitles = ts
         , emEMCreators = cs
         , emEMDates = ds
         }


extractEpubMeta :: (MonadIO m, MonadError String m) =>
   FilePath -> m EpubMeta
extractEpubMeta zipPath = do
   opfContents <- extractFileFromZip zipPath =<< opfPath zipPath
   let parsedDoc = readString hxtParseParams opfContents
         >>> propagateNamespaces

   --x <- liftIO $ runX ( parsedDoc >>> getTitles )
   --x <- liftIO $ runX ( parsedDoc >>> getCreators )
   --x <- liftIO $ runX ( parsedDoc >>> getDates )
   x <- liftIO $ runX ( parsedDoc >>> getMeta )

   return x
   --return ()


extractFileFromZip ::
   (MonadIO m, MonadError [Char] m) =>
   FilePath -> FilePath -> m String
extractFileFromZip zipPath filePath = do
   let dearchiver = "unzip"
   result <- liftIO $ tryEC $ run
      ((printf "%s -p %s %s" dearchiver zipPath filePath) :: String)
   case result of
      Left ps -> throwError $
         printf "[ERROR %s  zip file: %s  path in zip: %s  status: %s]"
            dearchiver zipPath filePath (show ps)
      Right output -> return output
