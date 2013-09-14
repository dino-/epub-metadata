-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of epub documents

   These data types were constructed by studying the IDPF OPF 
   specification for epub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Guide
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
