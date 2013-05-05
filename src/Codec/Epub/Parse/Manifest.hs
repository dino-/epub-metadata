-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the manifest from an EPUB file
module Codec.Epub.Parse.Manifest
   ( manifestP
   )
   where

import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Manifest
import Codec.Epub.Parse.Util


manifestItemP :: (ArrowXml a) => a (NTree XNode) ManifestItem
manifestItemP= atQTag (opfName "item") >>>
   proc x -> do
      i <- getAttrValue "id" -< x
      h <- getAttrValue "href" -< x
      m <- getAttrValue "media-type" -< x
      returnA -< ManifestItem i h m


manifestP :: (ArrowXml a) => a (NTree XNode) [ManifestItem]
manifestP = atQTag (opfName "manifest") >>>
   proc x -> do     
      l <- listA manifestItemP -< x
      returnA -< l
