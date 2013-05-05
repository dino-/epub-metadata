-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module ParseMetadata
   ( tests )
   where

import Control.Monad.Error
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testFull
   , testMinimal
   , testMissingAll
   ]


{- A fairly comprehensive test containing all possible things
   Not complete at this time because the library can't parse it all yet!
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "testFull.opf"
   actual <- runErrorT $ getMetadata xmlString
   let expected =
         Right Metadata
            { metaTitles =
               [ Title Nothing "Title Of This Book"
               , Title (Just "fr") "Titre De Ce Livre"
               ]
            , metaCreators = 
               [ Creator
                  (Just "aut")
                  (Just "Wiggins, Josephine B.")
                  "Josephine B. Wiggins"
               , Creator
                  (Just "aut")
                  Nothing
                  "Horatio Cromwell"
               , Creator
                  Nothing
                  Nothing
                  "Natalia Jenkins"
               ]
            , metaContributors = 
               [ Creator 
                  (Just "ill") 
                  (Just "Knickerbocker, Reginald Q.") 
                  "Reginald Q. Knickerbocker"
               , Creator 
                  (Just "edt") 
                  Nothing "Beverly Abercrombie"
               ]
            , metaSubjects = ["Fiction","Science Fiction"]
            , metaDescriptions =
               [ Description Nothing "This document is a stub used for unit testing. It is missing the rest of the tags that normally occur after metadata."
               , Description Nothing "An additional description"
               ]
            , metaPublishers =
               [ "Fictional Books Ltd."
               , "An additional publisher"
               ]
            , metaDates = 
               [ Date (Just "published") "2010"
               , Date (Just "created") "2010-05-07"
               , Date (Just "modified") "2010-05-08T10:20:57"
               , Date Nothing "2009"
               ]
            , metaTypes =
               [ "test OPF Package Document"
               , "an additional type"
               ]
            , metaFormats =
               [ "ePub publication"
               , "an additional format"
               ]
            , metaIds = 
               [ Identifier "isbn" (Just "ISBN") "1-82057-821-9"
               , Identifier "other" Nothing "1386506873266"
               ]
            , metaSources =
               [ "document source"
               , "an additional source"
               ]
            , metaLangs = ["en-US", "en-UK"]
            , metaRelations =
               [ "document relation"
               , "an additional relation"
               ]
            , metaCoverages =
               [ "coverage information"
               , "an additional coverage"
               ]
            , metaRights =
               [ "Copyright: 2010 Dino Morelli, License: BSD3"
               , "an additional rights"
               ]
            }
   assertEqual "very full" expected actual


{- Test the absolute minimum set of fields allowed while remaining 
   compliant with the spec
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- liftIO $ readFile $ "testsuite" </> "testMinimal.opf"
   actual <- runErrorT $ getMetadata xmlString
   let expected = 
         Right Metadata 
            { metaTitles = [Title Nothing "Title Of This Book"]
            , metaCreators = []
            , metaContributors = []
            , metaSubjects = []
            , metaDescriptions = []
            , metaPublishers = []
            , metaDates = []
            , metaTypes = []
            , metaFormats = []
            , metaIds = [Identifier "isbn" (Just "ISBN") "1-82057-821-9"]
            , metaSources = []
            , metaLangs = ["en-us"]
            , metaRelations = []
            , metaCoverages = []
            , metaRights = []
            }
   assertEqual "minimal" expected actual


{- Test data missing everything important: package version and 
   unique-identifier attributes, title, identifier and language tags
-}
testMissingAll :: Test
testMissingAll = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "testMissingAll.opf"
   actual <- runErrorT $ getMetadata xmlString
   let expected = Right emptyMetadata
   assertEqual "missing all" expected actual
