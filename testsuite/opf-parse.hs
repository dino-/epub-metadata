-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad.Error
import System.Exit
import System.FilePath
import Test.HUnit hiding ( counts )

import Codec.Epub2.IO
import Codec.Epub2.Opf.Package
import Codec.Epub2.Opf.Parse


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
   , testDamagedZip
   , testIllegalCharsBeforeDecl
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


{- Occasionally epub zip files come along that are damaged in this
   way. It's not fatal to the UNIX zip utility or to book readers, but had
   to be specially handled in the Haskell zip-archive library or it causes
   a fatal exception.
-}
testDamagedZip :: Test
testDamagedZip = TestLabel "damaged zip" $ TestCase $ do
   actual <- runErrorT $ opfContentsFromZip $ "testsuite"
      </> "damagedZipCentralDir.epub"
   actual @?= Left "Did not find end of central directory signature. Failed reading at byte position 138"


{- Found books coming from Barnes & Noble (for their NOOK reader) to
   contain illegal characters before the XML declaration. This is
   strictly not allowed by the XML specification. I am very
   disappointed with Barnes & Noble for selling garbage like this.
-}
testIllegalCharsBeforeDecl :: Test
testIllegalCharsBeforeDecl = TestCase $ do
   xmlString <- readFile $
      "testsuite" </> "testIllegalCharsBeforeDecl.opf"
   actual <- runErrorT $ parseXmlToOpf xmlString
   let expected =
         Right Package
            { opVersion = "2.0"
            , opUniqueId = "uuid_id"
            , opMeta = Metadata
               { metaTitles = [Title Nothing "Foo Bar Baz"]
               , metaCreators = []
               , metaContributors = []
               , metaSubjects = []
               , metaDescriptions = []
               , metaPublishers = []
               , metaDates = []
               , metaTypes = []
               , metaFormats = []
               , metaIds = [Identifier "uuid_id" (Just "uuid") "1122334455"]
               , metaSources = []
               , metaLangs = ["en"]
               , metaRelations = []
               , metaCoverages = []
               , metaRights = []}
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
   assertEqual "illegal chars before XML declaration" expected actual
