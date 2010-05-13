-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the metadata from an ePub file
module Codec.Epub.Opf.Metadata.Parse
   ( parseXmlToOpf
   , parseEpubOpf
   )
   where

import Control.Applicative
import Control.Monad.Error
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow

import Codec.Epub.IO
import Codec.Epub.Opf.Metadata


-- HXT helpers

atTag :: (ArrowXml a) => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

atQTag :: (ArrowXml a) => QName -> a (NTree XNode) XmlTree
atQTag tag = deep (isElem >>> hasQName tag)

text :: (ArrowXml a) => a (NTree XNode) String
text = getChildren >>> getText

notNullA :: (ArrowList a) => a [b] [b]
notNullA = isA $ not . null


mbQTagText :: (ArrowXml a) => QName -> a (NTree XNode) (Maybe String)
mbQTagText tag =
   ( atQTag tag >>>
     text >>> notNullA >>> arr Just )
   `orElse`
   (constA Nothing)


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

   Well, ok, the xml namespace URI will probably never change.
-}

dcName, opfName, xmlName :: String -> QName
dcName local = mkQName "dc" local "http://purl.org/dc/elements/1.1/"
opfName local = mkQName "opf" local "http://www.idpf.org/2007/opf"
xmlName local = mkQName "xml" local "http://www.w3.org/XML/1998/namespace"


getPackage :: (ArrowXml a) => a (NTree XNode) (String, String)
getPackage = atTag "package" >>>
   proc x -> do
      v <- getAttrValue "version" -< x
      u <- getAttrValue "unique-identifier" -< x
      returnA -< (v, u)


getTitle :: (ArrowXml a) => a (NTree XNode) EMTitle
getTitle = atQTag (dcName "title") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< EMTitle l c


{- Since creators and contributors have the same exact XML structure,
   this arrow is used to get either of them
-}
getCreator :: (ArrowXml a) => String -> a (NTree XNode) EMCreator
getCreator tag = atQTag (dcName tag) >>> ( unwrapArrow $ EMCreator
   <$> (WrapArrow $ mbGetQAttrValue (opfName "role"))
   <*> (WrapArrow $ mbGetQAttrValue (opfName "file-as"))
   <*> (WrapArrow $ text)
   )


getSubject :: (ArrowXml a) => a (NTree XNode) String
getSubject = atQTag (dcName "subject") >>> text


getDescription :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getDescription = mbQTagText $ dcName "description"


getPublisher :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getPublisher = mbQTagText $ dcName "publisher"


getDate :: (ArrowXml a) => a (NTree XNode) EMDate
getDate = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< EMDate e c


getType :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getType = mbQTagText $ dcName "type"


getFormat :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getFormat = mbQTagText $ dcName "format"


getId :: (ArrowXml a) => a (NTree XNode) EMId
getId = atQTag (dcName "identifier") >>>
   proc x -> do
      mbi <- mbGetAttrValue "id" -< x
      s <- mbGetQAttrValue (opfName "scheme") -< x
      c <- text -< x
      let i = maybe "[WARNING: missing required id attribute]" id mbi
      returnA -< EMId i s c


getSource :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getSource = mbQTagText $ dcName "source"


getLang :: (ArrowXml a) => a (NTree XNode) String
getLang = atQTag (dcName "language") >>> text


getRelation :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getRelation = mbQTagText $ dcName "relation"


getCoverage :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getCoverage = mbQTagText $ dcName "coverage"


getRights :: (ArrowXml a) => a (NTree XNode) (Maybe String)
getRights = mbQTagText $ dcName "rights"


getMeta :: (ArrowXml a) => a (NTree XNode) EpubMeta
getMeta = atTag "metadata" >>> ( unwrapArrow $ EpubMeta
   <$> (WrapArrow $ listA getTitle)
   <*> (WrapArrow $ listA $ getCreator "creator")
   <*> (WrapArrow $ listA $ getCreator "contributor")
   <*> (WrapArrow $ listA getSubject)
   <*> (WrapArrow $ getDescription)
   <*> (WrapArrow $ getPublisher)
   <*> (WrapArrow $ listA getDate)
   <*> (WrapArrow $ getType)
   <*> (WrapArrow $ getFormat)
   <*> (WrapArrow $ listA getId)
   <*> (WrapArrow $ getSource)
   <*> (WrapArrow $ listA getLang)
   <*> (WrapArrow $ getRelation)
   <*> (WrapArrow $ getCoverage)
   <*> (WrapArrow $ getRights)
   )


getBookData :: (ArrowXml a) => a (NTree XNode) OPFPackage
getBookData = 
   proc x -> do
      (v, u) <- getPackage -< x
      m <- getMeta -< x
      returnA -< OPFPackage v u m


{- | Extract the ePub metadata contained in the OPF Package Document 
   contained in the supplied string
-}
parseXmlToOpf :: (MonadIO m) => String -> m [OPFPackage]
parseXmlToOpf opfContents =
   liftIO $ runX (
      readString [(a_validate, v_0)] opfContents
      >>> propagateNamespaces
      >>> getBookData
      )


-- | Given the path to an ePub file, extract the metadata
parseEpubOpf :: (MonadIO m, MonadError String m) =>
   FilePath -> m OPFPackage
parseEpubOpf zipPath = do
   opfContents <- extractFileFromZip zipPath =<< opfPath zipPath
   result <- parseXmlToOpf opfContents

   case result of
      (em : []) -> return em
      _         -> throwError
         "ERROR: we didn't come up with a single EpubMeta"
