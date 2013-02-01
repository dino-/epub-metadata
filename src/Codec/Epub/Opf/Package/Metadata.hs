-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package.Metadata
   ( MetaTitle (..)
   , MetaCreator (..)
   , MetaDate (..)
   , MetaId (..)
   , Metadata (..)
   , emptyMetadata
   )
   where


-- | package\/metadata\/dc:title tag, xml:lang attr, content
data MetaTitle = MetaTitle (Maybe String) String
   deriving (Eq, Show)

{- | package\/metadata\/dc:creator tag, opf:role attr, opf:file-as attr,
   content
-}
data MetaCreator = MetaCreator (Maybe String) (Maybe String) String
   deriving (Eq, Show)

-- | package\/metadata\/dc:date tag, opf:event attr, content
data MetaDate = MetaDate (Maybe String) String
   deriving (Eq, Show)

{- | package\/metadata\/dc:identifier tag, id attr, opf:scheme attr,
   content
-}
data MetaId = MetaId String (Maybe String) String
   deriving (Eq, Show)

-- | package\/metadata tag
data Metadata = Metadata
   { metaTitles :: [MetaTitle]   -- ^ at least one required
   , metaCreators :: [MetaCreator]
   , metaContributors :: [MetaCreator]
   , metaSubjects :: [String]  -- ^ dc:subject tags
   , metaDescription :: Maybe String  -- ^ dc:description tags
   , metaPublisher :: Maybe String  -- ^ dc:publisher tag
   , metaDates :: [MetaDate]
   , metaType :: Maybe String  -- ^ dc:type tag
   , metaFormat :: Maybe String  -- ^ dc:format tag
   , metaIds :: [MetaId]          -- ^ at least one required
   , metaSource :: Maybe String  -- ^ dc:source tag
   , metaLangs :: [String]    -- ^ dc:language tags, at least one required
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
