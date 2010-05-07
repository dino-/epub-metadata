-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Opf.Metadata
   ( EMTitle (..)
   , EMCreator (..)
   , EMDate (..)
   , Id (..)
   , EpubMeta (..)
   , emptyEpubMeta
   )
   where


{- These data types were constructed by studying the IDPF OPF 
   specification  for ePub documents found here:

   http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html
-}

data EMTitle = EMTitle
   (Maybe String) -- xml:lang attribute
   String         -- content
   deriving Show

data EMCreator = EMCreator
   (Maybe String) -- opf:role attribute
   (Maybe String) -- opf:file-as attribute
   String         -- content
   deriving Show

data EMDate = EMDate
   (Maybe String) -- opf:event attribute
   String         -- content
   deriving Show

data Id = Id
   String         -- id attribute
   (Maybe String) -- opf:scheme attribute
   String         -- content
   deriving Show

data EpubMeta = EpubMeta
   { emEMTitles :: [EMTitle]   -- one required
   , emEMCreators :: [EMCreator]
   , emContributors :: [EMCreator]
   , emSubjects :: [String]
   , emDescription :: Maybe String
   , emPublisher :: Maybe String
   , emEMDates :: [EMDate]
   , emType :: Maybe String
   , emFormat :: Maybe String
   , emId :: [Id]          -- one required
   , emSource :: Maybe String
   , emLang :: [String]    -- one required
   , emRelation :: Maybe String
   , emCoverage :: Maybe String
   , emRights :: Maybe String
   }
   deriving Show

-- Note: This isn't valid as-is, some required values are empty lists!
emptyEpubMeta :: EpubMeta
emptyEpubMeta = EpubMeta
   { emEMTitles = []   -- one required
   , emEMCreators = []
   , emContributors = []
   , emSubjects = []
   , emDescription = Nothing
   , emPublisher = Nothing
   , emEMDates = []
   , emType = Nothing
   , emFormat = Nothing
   , emId = []       -- one required
   , emSource = Nothing
   , emLang = []     -- one required
   , emRelation = Nothing
   , emCoverage = Nothing
   , emRights = Nothing
   }
