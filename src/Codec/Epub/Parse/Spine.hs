-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the manifest from an EPUB file
module Codec.Epub.Parse.Spine
   ( spineP
   )
   where

import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Spine
import Codec.Epub.Parse.Util


spineItemrefP :: (ArrowXml a) => a (NTree XNode) SpineItemref
spineItemrefP = atQTag (opfName "itemref") >>>
   proc x -> do
      i <- getAttrValue "idref" -< x
      ml <- mbGetAttrValue "linear" -< x
      let l = maybe Nothing (\v -> if v == "no" then Just False else Just True) ml 
      returnA -< SpineItemref i l


spineP :: (ArrowXml a) => a (NTree XNode) Spine
spineP = atQTag (opfName "spine") >>>
   proc x -> do
      i <- getAttrValue "toc" -< x
      l <- listA spineItemrefP -< x
      returnA -< (Spine i l)
