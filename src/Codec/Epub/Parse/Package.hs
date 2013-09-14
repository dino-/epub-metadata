-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the package info from an epub file
module Codec.Epub.Parse.Package
   ( packageP
   )
   where

import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Package
import Codec.Epub.Parse.Util


packageP :: (ArrowXml a) => a (NTree XNode) Package
packageP = atQTag (opfName "package") >>>
   proc x -> do
      v <- getAttrValue "version" -< x
      u <- getAttrValue "unique-identifier" -< x
      returnA -< (Package v u)
