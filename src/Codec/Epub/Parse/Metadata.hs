-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the metadata from an EPUB file
module Codec.Epub.Parse.Metadata
   ( metadataP
   )
   where

import Control.Applicative
import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse.Util


titleP :: (ArrowXml a) => a (NTree XNode) (String, Title)
titleP = atQTag (dcName "title") >>>
   proc x -> do
      i <- mbGetAttrValue "id" -< x
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< ((maybe "" id i), Title l "" Nothing c)


{- Since creators and contributors have the same exact XML structure,
   this arrow is used to get either of them
-}
creatorP :: (ArrowXml a) => String -> a (NTree XNode) (String, Creator)
creatorP tag = atQTag (dcName tag) >>>
   proc x -> do
      i <- mbGetAttrValue "id" -< x
      r <- mbGetQAttrValue (opfName "role") -< x
      f <- mbGetQAttrValue (opfName "file-as") -< x
      t <- text -< x
      returnA -< ((maybe "" id i), Creator r f Nothing t)


subjectP :: (ArrowXml a) => a (NTree XNode) String
subjectP = atQTag (dcName "subject") >>> text


descriptionP :: (ArrowXml a) => a (NTree XNode) Description
descriptionP = atQTag (dcName "description") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< Description l c


publisherP :: (ArrowXml a) => a (NTree XNode) String
publisherP = atQTag (dcName "publisher") >>> text


dateP :: (ArrowXml a) => a (NTree XNode) Date
dateP = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< Date e c


typeP :: (ArrowXml a) => a (NTree XNode) (Maybe String)
typeP = mbQTagText $ dcName "type"


formatP :: (ArrowXml a) => a (NTree XNode) String
formatP = atQTag (dcName "format") >>> text


idP :: (ArrowXml a) => a (NTree XNode) Identifier
idP = atQTag (dcName "identifier") >>> ( unwrapArrow $ Identifier
   <$> (WrapArrow $ mbGetAttrValue "id")
   <*> (WrapArrow $ constA Nothing)
   <*> (WrapArrow $ mbGetQAttrValue (opfName "scheme"))  -- An attr in epub2
   <*> (WrapArrow $ text)
   )


sourceP :: (ArrowXml a) => a (NTree XNode) (Maybe String)
sourceP = mbQTagText $ dcName "source"


langP :: (ArrowXml a) => a (NTree XNode) String
langP = atQTag (dcName "language") >>> text


relationP :: (ArrowXml a) => a (NTree XNode) String
relationP = atQTag (dcName "relation") >>> text


coverageP :: (ArrowXml a) => a (NTree XNode) String
coverageP = atQTag (dcName "coverage") >>> text


rightsP :: (ArrowXml a) => a (NTree XNode) String
rightsP = atQTag (dcName "rights") >>> text


metadataP :: (ArrowXml a) => [Refinement] -> a (NTree XNode) Metadata
metadataP refinements =
   atQTag (opfName "metadata") >>> ( unwrapArrow $ Metadata
      <$> (WrapArrow $ listA $ idP >>. map (refineIdentifier refinements))
      <*> (WrapArrow $ listA $ titleP >>. map (refineTitle refinements))
      <*> (WrapArrow $ listA langP)
      <*> (WrapArrow $ listA $ creatorP "contributor" >>.
         map (refineCreator refinements))
      <*> (WrapArrow $ listA $ creatorP "creator" >>.
         map (refineCreator refinements))
      <*> (WrapArrow $ listA dateP)
      <*> (WrapArrow sourceP)
      <*> (WrapArrow typeP)
      <*> (WrapArrow $ listA coverageP)
      <*> (WrapArrow $ listA descriptionP)
      <*> (WrapArrow $ listA formatP)
      <*> (WrapArrow $ listA publisherP)
      <*> (WrapArrow $ listA relationP)
      <*> (WrapArrow $ listA rightsP)
      <*> (WrapArrow $ listA subjectP)
      )
