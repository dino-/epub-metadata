-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Codec.Epub.Opf.Metadata.Parse
   where

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


parseEpubMeta :: (MonadIO m, MonadError String m) =>
   FilePath -> m EpubMeta
parseEpubMeta zipPath = do
   opfContents <- extractFileFromZip zipPath =<< opfPath zipPath
   let parsedDoc = readString [(a_validate, v_0)] opfContents
         >>> propagateNamespaces

   --x <- liftIO $ runX ( parsedDoc >>> getTitles )
   --x <- liftIO $ runX ( parsedDoc >>> getCreators )
   --x <- liftIO $ runX ( parsedDoc >>> getDates )
   result <- liftIO $ runX ( parsedDoc >>> getMeta )

   case result of
      (em : []) -> return em
      _         -> throwError "ERROR: we didn't come up with a single EpubMeta"