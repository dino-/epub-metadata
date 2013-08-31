-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm>
-}
module Codec.Epub.Data.Metadata
   ( Refinement (..)
   , Creator (..)
   , Date (..)
   , Description (..)
   , Identifier (..)
   , Metadata (..)
   , Title (..)
   , emptyMetadata
   )
   where

import Data.List ( find )


{- | Refinements represent meta tags within the metadata section
     that refine other tags. These are used during the parsing phase
     and are discarded as their information is slotted into the
     data they refine (the types below like Creator, Title, etc..)
-}
data Refinement = Refinement
   { refId :: String
   , refProp :: String
   , refText :: String
   }


findByIdProp :: String -> String -> [Refinement] -> Maybe String
findByIdProp i prop = maybe Nothing (Just . refText) .
   find (\r -> refId r == i && refProp r == prop)


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


-- | package\/metadata\/dc:date tag, opf:event attr, content
data Date = Date (Maybe String) String
   deriving (Eq, Show)


-- | package\/metadata\/dc:description tag, xml:lang attr, content
data Description = Description (Maybe String) String
   deriving (Eq, Show)


{- | package\/metadata\/dc:identifier tag, id attr, opf:scheme attr,
   content
-}
data Identifier = Identifier String (Maybe String) String
   deriving (Eq, Show)


-- | package\/metadata\/dc:title tag
data Title = Title
   { titleLang :: Maybe String
   , titleType :: String
   , titleSeq :: Maybe Int
   , titleText :: String
   }
   deriving (Eq, Show)


{- For EPUB3, some information that is part of a title tag may be
   present in one or more meta tags. This function will merge these
   "refinements" into a list of Title data structures
-}
refineTitles :: [Refinement] -> [(String, Title)] -> [Title]
refineTitles refinements idTs = setMain . assignSeqs . assignTypes $ idTs
   where
      assignTypes = map (\(i, t) ->
         let newTy = maybe "" id $
               findByIdProp i "title-type" refinements
         in (i, t { titleType = newTy })
         )

      assignSeqs = map (\(i, t) ->
         let sq = maybe Nothing (Just . read) $
               findByIdProp i "display-seq" refinements
         in t { titleSeq = sq }
         )

      setMain ts = if mainExists ts
         then ts
         else (head ts) { titleType = "main" } : tail ts

         where mainExists = any (\t -> titleType t == "main")


-- | package\/metadata tag
data Metadata = Metadata
   { metaIds :: [Identifier]  -- ^ at least one required
   , metaTitles :: [Title]  -- ^ at least one required
   , metaLangs :: [String]  -- ^ dc:language tags, at least one required
   , metaContributors :: [Creator]
   , metaCreators :: [Creator]
   , metaDates :: [Date]
   -- , metaModified :: Maybe String
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
   -- , metaModified = Nothing
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
