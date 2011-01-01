-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Spine
   where

import Text.Printf

import Codec.Epub.Opf.Package.Spine


spineItemrefToString :: SpineItemref -> String
spineItemrefToString (SpineItemref idref linear) =
   printf "   idref: %s%s" idref (linearToString linear)

   where
      boolToYn True  = "yes"
      boolToYn False = "no"

      linearToString Nothing = ""
      linearToString (Just l) = printf ", linear: %s" (boolToYn l)


spineToString :: Spine -> String
spineToString (Spine toc itemRefs) =
   (printf "spine toc: %s, itemrefs:\n" toc) ++
   (unlines $ map spineItemrefToString itemRefs)
