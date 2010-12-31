-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package
   ( OPFPackage (..)
   , EMTitle (..)
   , EMCreator (..)
   , EMDate (..)
   , EMId (..)
   , Metadata (..)
   , EpubMFItem (..)
   , EpubSpine (..)
   , EpubSPItemRef (..)
   , EpubGuideRef (..)
   , emptyMetadata
   )
   where

import Codec.Epub.Opf.Package.Metadata


{- | opf:package tag

   Note that we are not yet storing the data that comes after
   \/package\/metadata in an OPF Package Document. But that may
   be added at a later time.
-}
data OPFPackage = OPFPackage
   { opVersion :: String  -- ^ version attr
   , opUniqueId :: String  -- ^ unique-identifier attr
   , opMeta :: Metadata  -- ^ metadata child element contents
   , opManifest :: [EpubMFItem] -- ^ manifest child element contents. one required
   , opSpine :: EpubSpine -- ^ spine child element contents
   , opGuide :: [EpubGuideRef] -- ^ guide child element contents
   }
   deriving (Eq, Show)

-- | manifest attributes (id, href, media-type)
type MFItemID = String
type MFItemHref = String
type MFItemMediaType = String

-- | opf:manifest tag
data EpubMFItem = EpubMFItem
   { emfID :: MFItemID
   , emfHref :: MFItemHref
   , emfMediaType :: MFItemMediaType
   } 
   deriving (Eq, Show)         

-- | opf:spine:itemref
data EpubSPItemRef = EpubSPItemRef
   { eiIdRef  :: MFItemID -- Must reference item in manifest
   , eiLinear :: Maybe Bool 
   }
   deriving (Eq, Show)

-- | opf:spine
data EpubSpine = EpubSpine 
   { esID    :: MFItemID  -- Must reference the NCX in the manifest
   , esItemrefs :: [ EpubSPItemRef ] -- one required
   }
   deriving (Eq, Show)

-- | opf:guide
data EpubGuideRef = EpubGuideRef
   { egType :: String -- Must follow 13th edition of the Chicago Manual of Style
   , egTitle :: Maybe String 
   , egHref :: String -- Must reference item in manifest
   }
   deriving (Eq, Show)
