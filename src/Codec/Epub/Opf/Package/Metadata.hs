-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package.Metadata
   ( Title (..)
   , Creator (..)
   , Date (..)
   , Identifier (..)
   , Metadata (..)
   , emptyMetadata
   )
   where


-- | package\/metadata\/dc:title tag, xml:lang attr, content
data Title = Title (Maybe String) String
   deriving (Eq, Show)

{- | package\/metadata\/dc:creator tag, opf:role attr, opf:file-as attr,
   content
-}
data Creator = Creator (Maybe String) (Maybe String) String
   deriving (Eq, Show)

-- | package\/metadata\/dc:date tag, opf:event attr, content
data Date = Date (Maybe String) String
   deriving (Eq, Show)

{- | package\/metadata\/dc:identifier tag, id attr, opf:scheme attr,
   content
-}
data Identifier = Identifier String (Maybe String) String
   deriving (Eq, Show)

-- | package\/metadata tag
data Metadata = Metadata
   { metaTitles :: [Title]  -- ^ at least one required
   , metaCreators :: [Creator]
   , metaContributors :: [Creator]
   , metaSubjects :: [String]  -- ^ dc:subject tags
   , metaDescription :: Maybe String  -- ^ dc:description tags
   , metaPublisher :: Maybe String  -- ^ dc:publisher tag
   , metaDates :: [Date]
   , metaType :: Maybe String  -- ^ dc:type tag
   , metaFormat :: Maybe String  -- ^ dc:format tag
   , metaIds :: [Identifier]  -- ^ at least one required
   , metaSource :: Maybe String  -- ^ dc:source tag
   , metaLangs :: [String]  -- ^ dc:language tags, at least one required
   , metaRelation :: Maybe String  -- ^ dc:relation tag
   , metaCoverage :: Maybe String  -- ^ dc:coverage tag
   , metaRights :: Maybe String  -- ^ dc:rights tag
   }
   deriving (Eq, Show)

-- | Note: This isn't valid as-is, some required values are empty lists!
emptyMetadata :: Metadata
emptyMetadata = Metadata
   { metaTitles = []   -- one required
   , metaCreators = []
   , metaContributors = []
   , metaSubjects = []
   , metaDescription = Nothing
   , metaPublisher = Nothing
   , metaDates = []
   , metaType = Nothing
   , metaFormat = Nothing
   , metaIds = []       -- one required
   , metaSource = Nothing
   , metaLangs = []     -- one required
   , metaRelation = Nothing
   , metaCoverage = Nothing
   , metaRights = Nothing
   }
