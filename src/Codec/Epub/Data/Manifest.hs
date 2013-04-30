-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Manifest
   ( ManifestItem (..)
   )
   where

import Codec.Epub.Data.Common


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
