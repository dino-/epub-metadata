-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Package
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

import Codec.Epub.Data.Guide
import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Spine


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
