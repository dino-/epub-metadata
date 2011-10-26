-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad.Error
import System.Exit
import System.FilePath
import Test.HUnit ( Counts (..), Test (..), assertEqual, runTestTT )
import Test.HUnit.Base ( Assertion )

import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse


main :: IO ()
main = do
   counts <- runTestTT tests
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitWith ExitSuccess
exit False = exitWith $ ExitFailure 1


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ e f) = (e == 0) && (f == 0)


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
   actual <- runErrorT $ parseXmlToOpf xmlString
   let expected =
         Right Package
            { opVersion = "2.0"
            , opUniqueId = "isbn"
            , opMeta = Metadata
               { metaTitles =
                  [ MetaTitle Nothing "Title Of This Book"
                  , MetaTitle (Just "fr") "Titre De Ce Livre"
                  ]
               , metaCreators = 
                  [ MetaCreator
                     (Just "aut")
                     (Just "Wiggins, Josephine B.")
                     "Josephine B. Wiggins"
                  , MetaCreator
                     (Just "aut")
                     Nothing
                     "Horatio Cromwell"
                  , MetaCreator
                     Nothing
                     Nothing
                     "Natalia Jenkins"
                  ]
               , metaContributors = 
                  [ MetaCreator 
                     (Just "ill") 
                     (Just "Knickerbocker, Reginald Q.") 
                     "Reginald Q. Knickerbocker"
                  , MetaCreator 
                     (Just "edt") 
                     Nothing "Beverly Abercrombie"
                  ]
               , metaSubjects = ["Fiction","Science Fiction"]
               , metaDescription = Just "This document is a stub used for unit testing. It is missing the rest of the tags that normally occur after metadata."
               , metaPublisher = Just "Fictional Books Ltd."
               , metaDates = 
                  [ MetaDate (Just "published") "2010"
                  , MetaDate (Just "created") "2010-05-07"
                  , MetaDate (Just "modified") "2010-05-08T10:20:57"
                  , MetaDate Nothing "2009"
                  ]
               , metaType = Just "test OPF Package Document"
               , metaFormat = Just "ePub publication"
               , metaIds = 
                  [ MetaId "isbn" (Just "ISBN") "1-82057-821-9"
                  , MetaId "other" Nothing "1386506873266"
                  ]
               , metaSource = Just "document source"
               , metaLangs = ["en-us"]
               , metaRelation = Just "document relation"
               , metaCoverage = Just "coverage information"
               , metaRights = Just "Copyright: 2010 Dino Morelli, License: BSD3"
               }
            , opManifest = 
               [ ManifestItem 
                  { mfiId = "ncx"
                  , mfiHref = "toc.ncx"
                  , mfiMediaType = "application/x-dtbncx+xml"
                  }
               , ManifestItem 
                  { mfiId = "titlePage"
                  , mfiHref = "content/titlePage.html"
                  , mfiMediaType = "application/xhtml+xml"
                  }
               , ManifestItem 
                  { mfiId = "someContent"
                  , mfiHref = "content/someContent.html"
                  , mfiMediaType = "application/xhtml+xml"
                  }
               ]
            , opSpine = Spine
               { spineToc = "ncx"
               , spineItemrefs = 
                  [ SpineItemref {siIdRef = "titlePage", siLinear = Nothing}
                  , SpineItemref {siIdRef = "someContent", siLinear = Nothing}
                  ]
               }
            , opGuide = 
               [ GuideRef 
                  { grType = "title-page"
                  , grTitle = Just "Title page"
                  , grHref = "content/titlePage.html"
                  }
               , GuideRef 
                  { grType = "text"
                  , grTitle = Just "Title Of This Book"
                  , grHref = "content/someContent.html"
                  }
               ]
            }
   assertEqual "very full" expected actual


{- Test the absolute minimum set of fields allowed while remaining 
   compliant with the spec
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- liftIO $ readFile $ "testsuite" </> "testMinimal.opf"
   actual <- runErrorT $ parseXmlToOpf xmlString
   let expected = 
         Right Package 
            { opVersion = "2.0"
            , opUniqueId = "isbn"
            , opMeta = Metadata 
               { metaTitles = [MetaTitle Nothing "Title Of This Book"]
               , metaCreators = []
               , metaContributors = []
               , metaSubjects = []
               , metaDescription = Nothing
               , metaPublisher = Nothing
               , metaDates = []
               , metaType = Nothing
               , metaFormat = Nothing
               , metaIds = [MetaId "isbn" (Just "ISBN") "1-82057-821-9"]
               , metaSource = Nothing
               , metaLangs = ["en-us"]
               , metaRelation = Nothing
               , metaCoverage = Nothing
               , metaRights = Nothing
               }
            , opManifest = 
               [ ManifestItem 
                  { mfiId = "ncx"
                  , mfiHref = "toc.ncx"
                  , mfiMediaType = "application/x-dtbncx+xml"
                  }
               ]
            , opSpine = Spine {spineToc = "ncx", spineItemrefs = []}
            , opGuide = []
            }
   assertEqual "minimal" expected actual


{- Test data missing everything important: package version and 
   unique-identifier attributes, title, identifier and language tags
-}
testMissingAll :: Test
testMissingAll = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "testMissingAll.opf"
   actual <- runErrorT $ parseXmlToOpf xmlString
   let expected =
         Right Package 
            { opVersion = ""
            , opUniqueId = ""
            , opMeta = emptyMetadata
            , opManifest = []
            , opSpine = Spine {spineToc = "", spineItemrefs = []}
            , opGuide = []
            }
   assertEqual "missing all" expected actual
