-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Metadata
   ( OPFPackage (..)
   , EMTitle (..)
   , EMCreator (..)
   , EMDate (..)
   , EMId (..)
   , EpubMeta (..)
   , emptyEpubMeta
   )
   where


{- | opf:package tag, version attr, unique-identifier attr

   Not really part of the metadata, but important enough to
   include here.
-}
data OPFPackage = OPFPackage String String
   deriving (Eq, Show)


-- | dc:title tag, xml:lang attr, content
data EMTitle = EMTitle (Maybe String) String
   deriving (Eq, Show)

-- | dc:creator tag, opf:role attr, opf:file-as attr, content
data EMCreator = EMCreator (Maybe String) (Maybe String) String
   deriving (Eq, Show)

-- | dc:date tag, opf:event attr, content
data EMDate = EMDate (Maybe String) String
   deriving (Eq, Show)

-- | dc:identifier tag, id attr, opf:scheme attr, content
data EMId = EMId String (Maybe String) String
   deriving (Eq, Show)

-- | opf:metadata tag
data EpubMeta = EpubMeta
   { emPackage :: OPFPackage
   , emTitles :: [EMTitle]   -- ^ at least one required
   , emCreators :: [EMCreator]
   , emContributors :: [EMCreator]
   , emSubjects :: [String]  -- ^ dc:subject tags
   , emDescription :: Maybe String  -- ^ dc:description tags
   , emPublisher :: Maybe String  -- ^ dc:publisher tag
   , emDates :: [EMDate]
   , emType :: Maybe String  -- ^ dc:type tag
   , emFormat :: Maybe String  -- ^ dc:format tag
   , emIds :: [EMId]          -- ^ at least one required
   , emSource :: Maybe String  -- ^ dc:source tag
   , emLangs :: [String]    -- ^ dc:language tags, at least one required
   , emRelation :: Maybe String  -- ^ dc:relation tag
   , emCoverage :: Maybe String  -- ^ dc:coverage tag
   , emRights :: Maybe String  -- ^ dc:rights tag
   }
   deriving (Eq, Show)

-- | Note: This isn't valid as-is, some required values are empty lists!
emptyEpubMeta :: EpubMeta
emptyEpubMeta = EpubMeta
   { emPackage = OPFPackage "" ""
   , emTitles = []   -- one required
   , emCreators = []
   , emContributors = []
   , emSubjects = []
   , emDescription = Nothing
   , emPublisher = Nothing
   , emDates = []
   , emType = Nothing
   , emFormat = Nothing
   , emIds = []       -- one required
   , emSource = Nothing
   , emLangs = []     -- one required
   , emRelation = Nothing
   , emCoverage = Nothing
   , emRights = Nothing
   }
