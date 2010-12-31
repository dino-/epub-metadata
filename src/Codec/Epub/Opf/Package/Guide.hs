-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package.Guide
   ( GuideRef (..)
   )
   where


-- | package\/guide\/reference tag
data GuideRef = GuideRef
   { grType :: String  -- ^ type attr. Must follow 13th edition of the Chicago Manual of Style
   , grTitle :: Maybe String  -- ^ title attr
   , grHref :: String  -- ^ href attr. Must reference item in manifest
   }
   deriving (Eq, Show)
