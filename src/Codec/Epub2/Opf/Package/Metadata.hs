-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub2.Opf.Package.Metadata
   ( Creator (..)
   , Date (..)
   , Description (..)
   , Identifier (..)
   , Metadata (..)
   , Title (..)
   , emptyMetadata
   )
   where


{- | package\/metadata\/dc:creator tag, opf:role attr, opf:file-as attr,
   content
-}
data Creator = Creator (Maybe String) (Maybe String) String
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

-- | package\/metadata\/dc:title tag, xml:lang attr, content
data Title = Title (Maybe String) String
   deriving (Eq, Show)

-- | package\/metadata tag
data Metadata = Metadata
   { metaTitles :: [Title]  -- ^ at least one required
   , metaCreators :: [Creator]
   , metaContributors :: [Creator]
   , metaSubjects :: [String]  -- ^ dc:subject tags
   , metaDescriptions :: [Description]
   , metaPublishers :: [String]  -- ^ dc:publisher tags
   , metaDates :: [Date]
   , metaTypes :: [String]  -- ^ dc:type tags
   , metaFormats :: [String]  -- ^ dc:format tags
   , metaIds :: [Identifier]  -- ^ at least one required
   , metaSources :: [String]  -- ^ dc:source tags
   , metaLangs :: [String]  -- ^ dc:language tags, at least one required
   , metaRelations :: [String]  -- ^ dc:relation tags
   , metaCoverages :: [String]  -- ^ dc:coverage tags
   , metaRights :: [String]  -- ^ dc:rights tags
   }
   deriving (Eq, Show)

-- | Note: This isn't valid as-is, some required values are empty lists!
emptyMetadata :: Metadata
emptyMetadata = Metadata
   { metaTitles = []  -- one required
   , metaCreators = []
   , metaContributors = []
   , metaSubjects = []
   , metaDescriptions = []
   , metaPublishers = []
   , metaDates = []
   , metaTypes = []
   , metaFormats = []
   , metaIds = []  -- one required
   , metaSources = []
   , metaLangs = []  -- one required
   , metaRelations = []
   , metaCoverages = []
   , metaRights = []
   }
