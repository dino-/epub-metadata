-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Archive
   ( tests )
   where

import Codec.Archive.Zip
import System.Directory
import System.FilePath
import Test.HUnit

import Codec.Epub.Archive


tests :: Test
tests = TestList
   [ testMkArchive
   ]


{- Test that the mimetype file is the first Entry in archives we create
-}
testMkArchive :: Test
testMkArchive = TestCase $ do
   origDir <- getCurrentDirectory
   a <- mkEpubArchive $ "testsuite" </> "bookfiles"
   setCurrentDirectory origDir
   let (firstFile : _) = filesInArchive a

   assertEqual "mimetype file is FIRST" "mimetype" firstFile
