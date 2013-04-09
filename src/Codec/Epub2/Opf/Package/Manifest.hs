-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub2.Opf.Package.Manifest
   ( ManifestItem (..)
   )
   where

import Codec.Epub2.Opf.Common


-- | manifest attribute values
type MFItemHref = String
type MFItemMediaType = String


-- | package\/manifest\/item tag
data ManifestItem = ManifestItem
   { mfiId :: MFItemId  -- ^ id attr
   , mfiHref :: MFItemHref  -- ^ href attr
   , mfiMediaType :: MFItemMediaType  -- ^ media-type attr
   }
   deriving (Eq, Show)
