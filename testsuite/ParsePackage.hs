-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module ParsePackage
   ( tests )
   where

import Control.Monad.Error
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Package
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testEpub2
   ]


{- Test reading the package info for an EPUB2 document
-}
testEpub2 :: Test
testEpub2 = TestCase $ do
   xmlString <- liftIO $ readFile $ "testsuite" </> "testMinimal.opf"
   actual <- runErrorT $ getPackage xmlString
   let expected = 
         Right Package 
            { opVersion = "2.0"
            , opUniqueId = "isbn"
            }
   assertEqual "epub2 package info" expected actual
