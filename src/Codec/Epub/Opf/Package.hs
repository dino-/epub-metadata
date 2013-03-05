-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package
   ( Package (..)
   , Title (..)
   , Creator (..)
   , Date (..)
   , Identifier (..)
   , Metadata (..)
   , ManifestItem (..)
   , SpineItemref (..)
   , Spine (..)
   , GuideRef (..)
   , emptyMetadata
   )
   where

import Codec.Epub.Opf.Package.Guide
import Codec.Epub.Opf.Package.Manifest
import Codec.Epub.Opf.Package.Metadata
import Codec.Epub.Opf.Package.Spine


{- | package tag
-}
data Package = Package
   { opVersion :: String  -- ^ version attr
   , opUniqueId :: String  -- ^ unique-identifier attr
   , opMeta :: Metadata  -- ^ metadata child element contents
   , opManifest :: [ManifestItem] -- ^ manifest child element contents. One required
   , opSpine :: Spine  -- ^ spine child element contents
   , opGuide :: [GuideRef] -- ^ guide child element contents
   }
   deriving (Eq, Show)
