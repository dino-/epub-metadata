-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Opf.Metadata
   ( EMTitle (..)
   , EMCreator (..)
   , EMDate (..)
   , EMId (..)
   , EpubMeta (..)
   , emptyEpubMeta
   )
   where


{- These data types were constructed by studying the IDPF OPF 
   specification for ePub documents found here:

   http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html
-}

data EMTitle = EMTitle
   (Maybe String) -- xml:lang attribute
   String         -- content
   deriving (Eq, Show)

data EMCreator = EMCreator
   (Maybe String) -- opf:role attribute
   (Maybe String) -- opf:file-as attribute
   String         -- content
   deriving (Eq, Show)

data EMDate = EMDate
   (Maybe String) -- opf:event attribute
   String         -- content
   deriving (Eq, Show)

data EMId = EMId
   String         -- id attribute
   (Maybe String) -- opf:scheme attribute
   String         -- content
   deriving (Eq, Show)

data EpubMeta = EpubMeta
   { emTitles :: [EMTitle]   -- one required
   , emCreators :: [EMCreator]
   , emContributors :: [EMCreator]
   , emSubjects :: [String]
   , emDescription :: Maybe String
   , emPublisher :: Maybe String
   , emDates :: [EMDate]
   , emType :: Maybe String
   , emFormat :: Maybe String
   , emIds :: [EMId]          -- one required
   , emSource :: Maybe String
   , emLangs :: [String]    -- one required
   , emRelation :: Maybe String
   , emCoverage :: Maybe String
   , emRights :: Maybe String
   }
   deriving (Eq, Show)

-- Note: This isn't valid as-is, some required values are empty lists!
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
