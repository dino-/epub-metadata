-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.FilePath
import Test.HUnit ( Counts, Test (..), assertEqual, runTestTT )
import Test.HUnit.Base ( Assertion )

import Codec.Epub.Opf.Metadata
import Codec.Epub.Opf.Metadata.Parse


main = runTestTT tests >> return ()


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
   actual <- parseXmlToOpf xmlString
   let expected = [ OPFPackage "2.0" "isbn" ( EpubMeta
         { emTitles =
            [ EMTitle Nothing "Title Of This Book"
            , EMTitle (Just "fr") "Titre De Ce Livre"
            ]
         , emCreators =
            [ EMCreator
               (Just "aut")
               (Just "Wiggins, Josephine B.")
               "Josephine B. Wiggins"
            , EMCreator
               (Just "aut")
               Nothing
               "Horatio Cromwell"
            , EMCreator
               Nothing
               Nothing
               "Natalia Jenkins"
            ]
         , emContributors =
            [ EMCreator
               (Just "ill")
               (Just "Knickerbocker, Reginald Q.")
               "Reginald Q. Knickerbocker"
            , EMCreator
               (Just "edt")
               Nothing
               "Beverly Abercrombie"
            ]
         , emSubjects = ["Fiction", "Science Fiction"]
         , emDescription = Just "This document is a stub used for unit testing. It is missing the rest of the tags that normally occur after metadata."
         , emPublisher = Just "Fictional Books Ltd."
         , emDates =
            [ EMDate (Just "published") "2010"
            , EMDate (Just "created") "2010-05-07"
            , EMDate (Just "modified") "2010-05-08T10:20:57"
            , EMDate Nothing "2009-08-03T16:22:20"
            ]
         , emType = Just "test OPF Package Document"
         , emFormat = Just "ePub publication"
         , emIds =
            [ EMId "isbn" (Just "ISBN") "1-82057-821-9"
            , EMId "other" Nothing "1386506873266"]
         , emSource = Just "document source"
         , emLangs = ["en-us"]
         , emRelation = Just "document relation"
         , emCoverage = Just "coverage information"
         , emRights = Just "Copyright: 2010 Dino Morelli, License: BSD3"
         } ) ]
   assertEqual "very full" expected actual


{- Test the absolute minimum set of fields allowed while remaining 
   compliant with the spec
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "testMinimal.opf"
   actual <- parseXmlToOpf xmlString
   let expected = [ OPFPackage "2.0" "isbn" ( EpubMeta
         { emTitles = [EMTitle Nothing "Title Of This Book"]
         , emCreators = []
         , emContributors = []
         , emSubjects = []
         , emDescription = Nothing
         , emPublisher = Nothing
         , emDates = []
         , emType = Nothing
         , emFormat = Nothing
         , emIds = [EMId "isbn" (Just "ISBN") "1-82057-821-9"]
         , emSource = Nothing
         , emLangs = ["en-us"]
         , emRelation = Nothing
         , emCoverage = Nothing
         , emRights = Nothing
         } ) ]
   assertEqual "minimal" expected actual


{- Test data missing everything important: package version and 
   unique-identifier attributes, title, identifier and language tags
-}
testMissingAll :: Test
testMissingAll = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "testMissingAll.opf"
   actual <- parseXmlToOpf xmlString
   let expected = [ OPFPackage "" "" emptyEpubMeta ]
   assertEqual "missing all" expected actual
