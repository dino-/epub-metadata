-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Epub3.ParseMetadata
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
   ]


{- A fairly comprehensive test containing all possible things
   Not complete at this time because the library can't parse it all yet!
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub3-full.opf"
   actual <- runErrorT $ getMetadata xmlString
   let expected =
         Right Metadata
            { metaIds =
               [ Identifier (Just "isbn") Nothing Nothing "1-82057-821-9"
               , Identifier (Just "other") (Just "some-type")
                  (Just "some-scheme") "1386506873266"
               ]
            , metaTitles =
               [ Title Nothing "main" (Just 1) "Title Of This Book"
               , Title Nothing "subtitle" (Just 2) "The Subtitle"
               , Title (Just "fr") "" Nothing "Titre De Ce Livre"
               ]
            , metaLangs = ["en-US", "en-UK"]
            , metaContributors =
               [ Creator
                  (Just "ill")
                  (Just "Knickerbocker, Reginald Q.")
                  Nothing
                  "Reginald Q. Knickerbocker"
               , Creator
                  (Just "edt")
                  Nothing
                  Nothing
                  "Beverly Abercrombie"
               ]
            , metaCreators =
               [ Creator
                  (Just "aut")
                  (Just "Wiggins, Josephine B.")
                  (Just 2)
                  "Josephine B. Wiggins"
               , Creator
                  (Just "aut")
                  Nothing
                  (Just 1)
                  "Horatio Cromwell"
               , Creator
                  Nothing
                  Nothing
                  (Just 3)
                  "Natalia Jenkins"
               ]
            , metaDates = [ Date Nothing "2012" ]
            , metaSource = Just "document source"
            , metaType = Just "test OPF Package Document"
            , metaCoverages =
               [ "coverage information"
               , "an additional coverage"
               ]
            , metaDescriptions =
               [ Description Nothing "This document is a stub used for unit testing. It is missing the rest of the tags that normally occur after metadata."
               , Description Nothing "An additional description"
               ]
            , metaFormats =
               [ "ePub publication"
               , "an additional format"
               ]
            , metaPublishers =
               [ "Fictional Books Ltd."
               , "An additional publisher"
               ]
            , metaRelations =
               [ "document relation"
               , "an additional relation"
               ]
            , metaSubjects = ["Fiction","Science Fiction"]
            , metaRights =
               [ "Copyright: 2010 Dino Morelli, License: BSD3"
               , "an additional rights"
               ]
            }

   assertEqual "epub3 - very full" expected actual
