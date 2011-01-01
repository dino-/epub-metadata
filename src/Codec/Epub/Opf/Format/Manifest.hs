-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Manifest
   where

import Text.Printf

import Codec.Epub.Opf.Package.Manifest


manifestItemToString :: ManifestItem -> String
manifestItemToString (ManifestItem mfId href mediaType) =
   printf "   id: %s, href: %s, media-type: %s" mfId href mediaType
