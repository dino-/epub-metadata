{-# LANGUAGE Arrows #-}

{- | Parsing for meta tags in the metadata section of the OPF
   Package XML Document
-}
module Codec.Epub.Parse.Refinements
   ( refinementsP
   )
   where

import Control.Applicative
import Control.Arrow.ListArrows
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse.Util


removeHash :: String -> String
removeHash ('#' : cs) = cs
removeHash s          = s


refinementP :: (ArrowXml a) => a (NTree XNode) Refinement
refinementP = atQTag (opfName "meta") >>> ( unwrapArrow $ Refinement
   <$> (WrapArrow $ mbGetAttrValue "refines" >>^ maybe "" removeHash)
   <*> (WrapArrow $ mbGetAttrValue "property" >>^ maybe "" id)
   <*> (WrapArrow $ mbGetAttrValue "scheme" >>^ maybe "" id)
   <*> (WrapArrow $ text)
   )


refinementsP :: (ArrowXml a) => a (NTree XNode) [Refinement]
refinementsP = atQTag (opfName "metadata") >>> listA refinementP
