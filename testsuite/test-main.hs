-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Exit
import Test.HUnit hiding ( counts )

import qualified Archive
--import qualified OpfParse
import qualified ParseMetadata
import qualified ParsePackage
import qualified ParseManifest
import qualified ParseSpine


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
   [ Archive.tests
   , ParsePackage.tests
   , ParseMetadata.tests
   , ParseManifest.tests
   , ParseSpine.tests
   -- , OpfParse.tests
   ]
