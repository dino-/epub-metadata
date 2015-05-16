-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Epub2.ParseGuide
   ( tests )
   where

import Control.Monad.Except
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Guide
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testFull
   , testMinimal
   ]


{- A test containing a couple of guide ref items
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub2-full.opf"
   actual <- runExceptT $ getGuide xmlString
   let expected =
         Right [ GuideRef
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
   assertEqual "some guide items" expected actual


{- Minimal guide
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- liftIO $ readFile $ "testsuite" </> "epub2-minimal.opf"
   actual <- runExceptT $ getGuide xmlString
   let expected = Right []
   assertEqual "minimal guide" expected actual
