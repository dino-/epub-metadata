-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Codec.Epub.Opf.Metadata
   where

import Control.Monad.Error
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
   String         -- id
   (Maybe String) -- opf:scheme
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

hxtParseParams = [(a_validate, v_0)]

atTag tag = deep (isElem >>> hasName tag)

atQTag tag = deep (isElem >>> hasQName tag)

text = getChildren >>> getText

notNullA = isA $ not . null

mbGetAttrValue n =
   (getAttrValue n >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)

mbGetQAttrValue qn =
   (getQAttrValue qn >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)


-- ePub parsing helpers

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


getTitles = atQTag (dcName "title") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< EMTitle l c


getCreators = atQTag (dcName "creator") >>>
   proc x -> do
      r <- mbGetQAttrValue (opfName "role") -< x
      f <- mbGetQAttrValue (opfName "file-as") -< x
      c <- text -< x
      returnA -< EMCreator r f c


getDates = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< EMDate e c


getMeta = atTag "metadata" >>>
   proc x -> do
      ts <- getTitles -< x
      cs <- getCreators -< x
      ds <- getDates -< x
      returnA -< emptyEpubMeta
         { emEMTitles = ts
         , emEMCreators = cs
         , emEMDates = ds
         }


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
