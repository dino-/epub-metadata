-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of epub documents

   Both versions of epub (2.x and 3.x) are supported by this software.

   These data types were constructed by studying the IDPF
   specifications for epub documents found here

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>

   and here

   <http://www.idpf.org/epub/30/spec/epub30-publications.html>
-}
module Codec.Epub.Data.Metadata
   ( Refinement (..)
   , Identifier (..)
   , Title (..)
   , Creator (..)
   , Date (..)
   , Description (..)
   , Metadata (..)
   , emptyMetadata
   , refineIdentifier
   , refineTitle
   , getModified
   , refineCreator
   )
   where

import Control.Monad ( mplus )
import Data.List ( find )


{- | Refinements represent meta tags within the metadata section
     that refine other tags. These are used during the parsing phase
     and are discarded as their information is slotted into the
     data they refine (the types below like Creator, Title, etc..)
-}
data Refinement = Refinement
   { refId :: String  -- ^ id attribute
   , refProp :: String  -- ^ property attribute
   , refScheme :: String  -- ^ scheme attribute
   , refText :: String  -- ^ content
   }


findByIdProp :: String -> String -> [Refinement] -> Maybe Refinement
findByIdProp i prop = find (\r -> refId r == i && refProp r == prop)


-- | package\/metadata\/dc:identifier tag
data Identifier = Identifier
   { idId :: Maybe String  -- ^ id attribute
   , idType :: Maybe String  -- ^ identifier-type property from meta tag
   , idScheme :: Maybe String  -- ^ scheme from attribute or meta tag
   , idText :: String  -- ^ identifier text
   }
   deriving (Eq, Show)


refineIdentifier :: [Refinement] -> Identifier -> Identifier
refineIdentifier refinements ident = assignScheme . assignType $ ident
   where
      meta = findByIdProp (maybe "" id $ idId ident)
         "identifier-type" refinements

      assignType ident' = ident' { idType = refText `fmap` meta }

      assignScheme ident' =
         let existingScheme = idScheme ident'
         in ident' { idScheme = existingScheme `mplus`
               (refScheme `fmap` meta) }


-- | package\/metadata\/dc:title tag
data Title = Title
   { titleLang :: Maybe String  -- ^ lang attributed
   , titleType :: Maybe String  -- ^ title-type property from meta tag
   , titleSeq :: Maybe Int  -- ^ display-sequence property from meta
   , titleText :: String  -- ^ title text
   }
   deriving (Eq, Show)


{- For EPUB3, some information that is part of a title tag may be
   present in one or more meta tags. This function will merge any 
   relevent "refinements" into a Title data structure
-}
refineTitle :: [Refinement] -> (String, Title) -> Title
refineTitle refinements (elid, title) = assignSeq . assignType $ title
   where
      assignType title' =
         let newTy = refText `fmap`
               findByIdProp elid "title-type" refinements
         in title' { titleType = newTy }

      assignSeq title' =
         let sq = maybe Nothing (Just . read . refText) $
               findByIdProp elid "display-seq" refinements
         in title' { titleSeq = sq }


{- | package\/metadata\/dc:creator or package\/metadata\/dc:contributor
   tags
-}
data Creator = Creator
   { creatorRole :: Maybe String
   , creatorFileAs :: Maybe String
   , creatorSeq :: Maybe Int
   , creatorText :: String
   }
   deriving (Eq, Show)


refineCreator :: [Refinement] -> (String, Creator) -> Creator
refineCreator refinements (elid, creator) =
   assignSeq . assignFileAs . assignRole $ creator

   where
      assignRole creator' =
         let existingRole = creatorRole creator'
             metaRole = maybe Nothing (Just . refText) $
               findByIdProp elid "role" refinements
         in creator' { creatorRole = existingRole `mplus` metaRole }

      assignFileAs creator' =
         let existingFileAs = creatorFileAs creator'
             metaFileAs = maybe Nothing (Just . refText) $
               findByIdProp elid "file-as" refinements
         in creator' { creatorFileAs = existingFileAs `mplus` metaFileAs }

      assignSeq creator' =
         let sq = maybe Nothing (Just . read . refText) $
               findByIdProp elid "display-seq" refinements
         in creator' { creatorSeq = sq }


-- | package\/metadata\/dc:date tag, opf:event attr, content
data Date = Date (Maybe String) String
   deriving (Eq, Show)


getModified :: [Refinement] -> Maybe String
getModified refinements =
   refText `fmap` findByIdProp "" "dcterms:modified" refinements


-- | package\/metadata\/dc:description tag, xml:lang attr, content
data Description = Description (Maybe String) String
   deriving (Eq, Show)


-- | package\/metadata tag
data Metadata = Metadata
   { metaIds :: [Identifier]  -- ^ at least one required
   , metaTitles :: [Title]  -- ^ at least one required
   , metaLangs :: [String]  -- ^ dc:language tags, at least one required
   , metaContributors :: [Creator]
   , metaCreators :: [Creator]
   , metaDates :: [Date]
   , metaModified :: Maybe String  -- ^ meta tag with property dcterms:modified, present in epub3 documents
   , metaSource :: Maybe String  -- ^ dc:source tags
   , metaType :: Maybe String  -- ^ dc:type tags
   , metaCoverages :: [String]  -- ^ dc:coverage tags
   , metaDescriptions :: [Description]
   , metaFormats :: [String]  -- ^ dc:format tags
   , metaPublishers :: [String]  -- ^ dc:publisher tags
   , metaRelations :: [String]  -- ^ dc:relation tags
   , metaRights :: [String]  -- ^ dc:rights tags
   , metaSubjects :: [String]  -- ^ dc:subject tags
   }
   deriving (Eq, Show)

-- | Note: This isn't valid as-is, some required values are empty lists!
emptyMetadata :: Metadata
emptyMetadata = Metadata
   { metaIds = []  -- one required
   , metaTitles = []  -- one required
   , metaLangs = []  -- one required
   , metaContributors = []
   , metaCreators = []
   , metaDates = []
   , metaModified = Nothing
   , metaSource = Nothing
   , metaType = Nothing
   , metaCoverages = []
   , metaDescriptions = []
   , metaFormats = []
   , metaPublishers = []
   , metaRelations = []
   , metaRights = []
   , metaSubjects = []
   }
