-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module ParseManifest
   ( tests )
   where

import Control.Monad.Error
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Manifest
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testSeveral
   , testMissing
   ]


{- A fairly comprehensive test containing several manifest items
-}
testSeveral :: Test
testSeveral = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub2-full.opf"
   actual <- runErrorT $ getManifest xmlString
   let expected =
         Right $ Manifest [ ManifestItem 
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
   assertEqual "several manifest items" expected actual


{- Test data devoid of any manifest items
-}
testMissing :: Test
testMissing = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub2-missingAll.opf"
   actual <- runErrorT $ getManifest xmlString
   let expected =
         Right $ Manifest []
   assertEqual "missing entirely" expected actual
