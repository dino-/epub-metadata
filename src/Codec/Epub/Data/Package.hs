-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data type for working with epub package info

   This data type was constructed by studying the IDPF OPF 
   specification for epub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Package
   ( Package (..)
   )
   where


{- | package tag
-}
data Package = Package
   { pkgVersion :: String  -- ^ version attribute. This contains which epub specification version (2.x or 3.x) this document conforms to.
   , pkgUniqueId :: String  -- ^ unique-identifier attribute. This should relate to one metaIdentifier in the Metadata and is intended to represent this book's unique identifier.
   }
   deriving (Eq, Show)
