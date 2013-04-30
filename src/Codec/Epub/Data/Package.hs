-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data type for working with EPUB package info

   This data type was constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Package
   ( Package (..)
   )
   where


{- | package tag
-}
data Package = Package
   { opVersion :: String  -- ^ version attr
   , opUniqueId :: String  -- ^ unique-identifier attr
   }
   deriving (Eq, Show)
