-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- | Data types for working with the metadata of ePub documents

   These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   <http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html>
-}
module Codec.Epub.Opf.Package
   ( OPFPackage (..)
   , EMTitle (..)
   , EMCreator (..)
   , EMDate (..)
   , EMId (..)
   , EpubMeta (..)
   , EpubMFItem (..)
   , EpubSpine (..)
   , EpubSPItemRef (..)
   , EpubGuideRef (..)
   , emptyEpubMeta
   )
   where


{- | opf:package tag

   Note that we are not yet storing the data that comes after
   \/package\/metadata in an OPF Package Document. But that may
   be added at a later time.
-}
data OPFPackage = OPFPackage
   { opVersion :: String  -- ^ version attr
   , opUniqueId :: String  -- ^ unique-identifier attr
   , opMeta :: EpubMeta  -- ^ metadata child element contents
   , opManifest :: [EpubMFItem] -- ^ manifest child element contents. one required
   , opSpine :: EpubSpine -- ^ spine child element contents
   , opGuide :: [EpubGuideRef] -- ^ guide child element contents
   }
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

-- | manifest attributes (id, href, media-type)
type MFItemID = String
type MFItemHref = String
type MFItemMediaType = String

-- | opf:manifest tag
data EpubMFItem = EpubMFItem
   { emfID :: MFItemID
   , emfHref :: MFItemHref
   , emfMediaType :: MFItemMediaType
   } 
   deriving (Eq, Show)         

-- | opf:spine:itemref
data EpubSPItemRef = EpubSPItemRef
   { eiIdRef  :: MFItemID -- Must reference item in manifest
   , eiLinear :: Maybe Bool 
   }
   deriving (Eq, Show)

-- | opf:spine
data EpubSpine = EpubSpine 
   { esID    :: MFItemID  -- Must reference the NCX in the manifest
   , esItemrefs :: [ EpubSPItemRef ] -- one required
   }
   deriving (Eq, Show)

-- | opf:guide
data EpubGuideRef = EpubGuideRef
   { egType :: String -- Must follow 13th edition of the Chicago Manual of Style
   , egTitle :: Maybe String 
   , egHref :: String -- Must reference item in manifest
   }
   deriving (Eq, Show)

-- | opf:metadata tag
data EpubMeta = EpubMeta
   { emTitles :: [EMTitle]   -- ^ at least one required
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
   { emTitles = []   -- one required
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
