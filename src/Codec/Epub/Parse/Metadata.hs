-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the metadata from an EPUB file
module Codec.Epub.Parse.Metadata
   ( getMetadata
   )
   where

import Control.Applicative
import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse.Common


getTitle :: (ArrowXml a) => a (NTree XNode) Title
getTitle = atQTag (dcName "title") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< Title l c


{- Since creators and contributors have the same exact XML structure,
   this arrow is used to get either of them
-}
getCreator :: (ArrowXml a) => String -> a (NTree XNode) Creator
getCreator tag = atQTag (dcName tag) >>> ( unwrapArrow $ Creator
   <$> (WrapArrow $ mbGetQAttrValue (opfName "role"))
   <*> (WrapArrow $ mbGetQAttrValue (opfName "file-as"))
   <*> (WrapArrow $ text)
   )


getSubject :: (ArrowXml a) => a (NTree XNode) String
getSubject = atQTag (dcName "subject") >>> text


getDescription :: (ArrowXml a) => a (NTree XNode) Description
getDescription = atQTag (dcName "description") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< Description l c


getPublisher :: (ArrowXml a) => a (NTree XNode) String
getPublisher = atQTag (dcName "publisher") >>> text


getDate :: (ArrowXml a) => a (NTree XNode) Date
getDate = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< Date e c


getType :: (ArrowXml a) => a (NTree XNode) String
getType = atQTag (dcName "type") >>> text


getFormat :: (ArrowXml a) => a (NTree XNode) String
getFormat = atQTag (dcName "format") >>> text


getId :: (ArrowXml a) => a (NTree XNode) Identifier
getId = atQTag (dcName "identifier") >>>
   proc x -> do
      mbi <- mbGetAttrValue "id" -< x
      s <- mbGetQAttrValue (opfName "scheme") -< x
      c <- text -< x
      let i = maybe "[WARNING: missing required id attribute]" id mbi
      returnA -< Identifier i s c


getSource :: (ArrowXml a) => a (NTree XNode) String
getSource = atQTag (dcName "source") >>> text


getLang :: (ArrowXml a) => a (NTree XNode) String
getLang = atQTag (dcName "language") >>> text


getRelation :: (ArrowXml a) => a (NTree XNode) String
getRelation = atQTag (dcName "relation") >>> text


getCoverage :: (ArrowXml a) => a (NTree XNode) String
getCoverage = atQTag (dcName "coverage") >>> text


getRights :: (ArrowXml a) => a (NTree XNode) String
getRights = atQTag (dcName "rights") >>> text


getMetadata :: (ArrowXml a) => a (NTree XNode) Metadata
getMetadata = atQTag (opfName "metadata") >>> ( unwrapArrow $ Metadata
   <$> (WrapArrow $ listA getTitle)
   <*> (WrapArrow $ listA $ getCreator "creator")
   <*> (WrapArrow $ listA $ getCreator "contributor")
   <*> (WrapArrow $ listA getSubject)
   <*> (WrapArrow $ listA getDescription)
   <*> (WrapArrow $ listA getPublisher)
   <*> (WrapArrow $ listA getDate)
   <*> (WrapArrow $ listA getType)
   <*> (WrapArrow $ listA getFormat)
   <*> (WrapArrow $ listA getId)
   <*> (WrapArrow $ listA getSource)
   <*> (WrapArrow $ listA getLang)
   <*> (WrapArrow $ listA getRelation)
   <*> (WrapArrow $ listA getCoverage)
   <*> (WrapArrow $ listA getRights)
   )
