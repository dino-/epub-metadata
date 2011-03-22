-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Archive
   ( mkEpubArchive
   , writeArchive
   )
   where

import Codec.Archive.Zip
import Control.Monad
import qualified Data.ByteString.Lazy as B
import System.Directory


{- | Construct a zip Archive containing epub book data from the 
     specified directory
-}
mkEpubArchive :: FilePath -> IO Archive
mkEpubArchive rootDir = do
   setCurrentDirectory rootDir
   foldM (flip id) emptyArchive
      [ flip (addFilesToArchive []) ["mimetype"]
      , flip (addFilesToArchive [OptRecursive]) ["."]
      ]


-- | Write a zip Archive to disk using the specified filename
writeArchive :: FilePath -> Archive -> IO ()
writeArchive zipPath = (B.writeFile zipPath) . fromArchive
