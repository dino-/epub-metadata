-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package.Spine
   ( Spine (..)
   , SpineItemref (..)
   )
   where

import Codec.Epub.Opf.Common


-- | opf:spine:itemref
data SpineItemref = SpineItemref
   { siIdRef  :: MFItemID -- Must reference item in manifest
   , siLinear :: Maybe Bool
   }
   deriving (Eq, Show)


-- | opf:spine
data Spine = Spine
   { spineId    :: MFItemID  -- Must reference the NCX in the manifest
   , spineItemrefs :: [ SpineItemref ] -- one required
   }
   deriving (Eq, Show)
