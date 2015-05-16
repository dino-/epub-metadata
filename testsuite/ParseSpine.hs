-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module ParseSpine
   ( tests )
   where

import Control.Monad.Error
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Spine
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testFull
   , testMinimal
   , testMissingAll
   ]


{- A test containing a couple of spine items
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub2-full.opf"
   actual <- runErrorT $ getSpine xmlString
   let expected =
         Right Spine
            { spineToc = "ncx"
            , spineItemrefs = 
               [ SpineItemref {siIdRef = "titlePage", siLinear = Nothing}
               , SpineItemref {siIdRef = "someContent", siLinear = Nothing}
               ]
            }
   assertEqual "some spine items" expected actual


{- Minimal spine, only the spineToc is required
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- liftIO $ readFile $ "testsuite" </> "epub2-minimal.opf"
   actual <- runErrorT $ getSpine xmlString
   let expected = Right Spine {spineToc = "ncx", spineItemrefs = []}
   assertEqual "minimal" expected actual


{- Missing any spine info at all
-}
testMissingAll :: Test
testMissingAll = TestCase $ do
   xmlString <- readFile $ "testsuite" </> "epub2-missingAll.opf"
   actual <- runErrorT $ getSpine xmlString
   let expected = Right Spine {spineToc = "", spineItemrefs = []}
   assertEqual "missing all" expected actual
