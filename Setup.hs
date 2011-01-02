#! /usr/bin/env runhaskell

-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( unless )
import Distribution.Simple
import System.Cmd ( system )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath
import System.Posix.Files ( createSymbolicLink, fileExist )


main = defaultMainWithHooks (simpleUserHooks 
   { postBuild = customPostBuild
   , runTests = testRunner
   } )

   where
      -- Create symlink to the binary after build for developer 
      -- convenience
      customPostBuild _ _ _ _ = do
         let binDir = "bin"
         let binName = "epub-meta"
         let destPath = binDir </> binName

         createDirectoryIfMissing True binDir

         linkExists <- fileExist destPath
         unless linkExists $ do
            createSymbolicLink
               (".." </> "dist" </> "build" </> binName </> binName)
               destPath

      -- Target for running all unit tests
      testRunner _ _ _ _ = do
         system $ "runhaskell -isrc testsuite/runtests.hs"
         return ()
