{- | Data types for working with the metadata of epub documents

   This module defines the Metadata structure which contains most of the taxonomic information about the literary work. Metadata is probably the most important data structure in this library.

   Both commonly-used versions of epub (2.x and 3.x) are supported by these types.
-}
module Codec.Epub.Data.Metadata
   ( Metadata (..)
   , Identifier (..)
   , Title (..)
   , Creator (..)
   , Date (..)
   , DateEvent (..)
   , Description (..)
   , Refinement (..)
   , dateEventFromString
   , dateEventToString
   , emptyMetadata
   , refineIdentifier
   , refineTitle
   , refineCreator
   )
   where

import Control.Monad ( mplus )
import Data.List ( find )
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )


{- | Refinements represent meta tags within the metadata section
   that refine other tags. These are used during the parsing phase
   and are discarded as their information is slotted into the data
   they refine (CreatorS, TitleS, IdentifierS, etc..)

   This is specific to epub3
-}
data Refinement = Refinement
   { refId :: String  -- ^ id attribute
   , refProp :: String  -- ^ property attribute
   , refScheme :: String  -- ^ scheme attribute
   , refText :: String  -- ^ meta tag text
   }


{- Used for locating specific meta information in a list of
   Refinements. The data from these meta tags is related to other
   tags by an XML id attribute.
-}
findByIdProp :: String -> String -> [Refinement] -> Maybe Refinement
findByIdProp i prop = find (\r -> refId r == i && refProp r == prop)


-- | package\/metadata\/dc:identifier tag
data Identifier = Identifier
   { idId :: Maybe String  -- ^ id attribute
   , idType :: Maybe String  -- ^ identifier-type property from meta tag
   , idScheme :: Maybe String  -- ^ scheme from attribute or meta tag
   , idText :: String  -- ^ identifier tag text
   }
   deriving (Eq, Show)


{- | Used internally by Codec.Epub.Parse.Metadata to merge epub3 meta
   tag info into the data gathered from an identifier tag
-}
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
   { titleLang :: Maybe String  -- ^ lang attribute
   , titleType :: Maybe String  -- ^ title-type property from meta tag
   , titleSeq :: Maybe Int  -- ^ display-sequence property from meta
   , titleText :: String  -- ^ title tag text
   }
   deriving (Eq, Show)


{- | Used internally by Codec.Epub.Parse.Metadata to merge epub3 meta
   tag info into the data gathered from a title tag
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

   This structure is used for both contributor and creator as they are exactly the same.
-}
data Creator = Creator
   { creatorRole :: Maybe String  -- ^ role from attribute or meta tag
   , creatorFileAs :: Maybe String  -- ^ file-as from attribute or meta tag
   , creatorSeq :: Maybe Int  -- ^ display-sequence property from meta
   , creatorText :: String  -- ^ creator or contributor tag text
   }
   deriving (Eq, Show)


{- | Used internally by Codec.Epub.Parse.Metadata to merge epub3 meta
   tag info into the data gathered from contributor and creator tags
-}
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


data DateEvent
  = Available
  | Created
  | DateAccepted
  | DateCopyrighted
  | DateSubmitted
  | Epub
  | Issued
  | Modified
  | Valid
  deriving (Eq, Ord, Show)

dateEventFromString :: Maybe String -> DateEvent
dateEventFromString (Just "dcterms:available") = Available
dateEventFromString (Just "dcterms:created") = Created
dateEventFromString (Just "publication") = Created                      -- EPUB 2.x
dateEventFromString (Just "dcterms:dateAccepted") = DateAccepted
dateEventFromString (Just "dcterms:dateCopyrighted") = DateCopyrighted
dateEventFromString (Just "dcterms:dateSubmitted") = DateSubmitted
dateEventFromString (Just "dcterms:issued") = Issued
dateEventFromString (Just "original-publication") = Issued              -- EPUB 2.x
dateEventFromString (Just "dcterms:modified") = Modified
dateEventFromString (Just "dcterms:Valid") = Valid
dateEventFromString _ = Epub

dateEventToString :: DateEvent -> String
dateEventToString Available = "available"
dateEventToString Created = "created"
dateEventToString DateAccepted = "dateAccepted"
dateEventToString DateCopyrighted = "dateCopyrighted"
dateEventToString DateSubmitted = "dateSubmitted"
dateEventToString Epub = "EPUB created"
dateEventToString Issued = "issued"
dateEventToString Modified = "modified"
dateEventToString Valid = "valid"


-- | EPUB 2.x: package\/metadata\/dc:date tag, opf:event attribute, text
-- | EPUB 3.x: package\/metadata\/dc:date tag
-- |           package\/metadata\/meta property="dcterms:issued"
-- |           package\/metadata\/meta property="dcterms:modified"
-- |           package\/metadata\/meta property="dcterms:..."
newtype Date = Date String
   deriving (Eq, Show)


-- | package\/metadata\/dc:description tag, xml:lang attribute, text
data Description = Description (Maybe String) String
   deriving (Eq, Show)


{- | package\/metadata tag

   This is perhaps the most useful data structure in this library. It
   contains most of the information tools will want to use to
   organize epub documents.
-}
data Metadata = Metadata
   { metaIds :: [Identifier]  -- ^ at least one required
   , metaTitles :: [Title]  -- ^ at least one required
   , metaLangs :: [String]  -- ^ dc:language tags, at least one required
   , metaContributors :: [Creator]
   , metaCreators :: [Creator]
   , metaDates :: Map DateEvent Date
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
   , metaDates = Map.empty
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
