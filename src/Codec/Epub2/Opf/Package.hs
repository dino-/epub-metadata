-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub2.Opf.Package
   ( Package (..)
   , Creator (..)
   , Date (..)
   , Description (..)
   , Identifier (..)
   , Metadata (..)
   , Title (..)
   , ManifestItem (..)
   , SpineItemref (..)
   , Spine (..)
   , GuideRef (..)
   , emptyMetadata
   )
   where

import Codec.Epub2.Opf.Package.Guide
import Codec.Epub2.Opf.Package.Manifest
import Codec.Epub2.Opf.Package.Metadata
import Codec.Epub2.Opf.Package.Spine


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
