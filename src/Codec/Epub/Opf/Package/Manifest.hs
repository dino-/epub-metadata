-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package.Manifest
   ( ManifestItem (..)
   )
   where

import Codec.Epub.Opf.Common


-- | manifest attributes
type MFItemHref = String
type MFItemMediaType = String


-- | opf:manifest tag
data ManifestItem = ManifestItem
   { mfiID :: MFItemID
   , mfiHref :: MFItemHref
   , mfiMediaType :: MFItemMediaType
   }
   deriving (Eq, Show)
