-- Copyright: 2011-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub2.Archive
   ( mkEpubArchive
   , readArchive
   , writeArchive
   )
   where

import Codec.Archive.Zip
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import System.Directory
import System.FilePath


{- Recursively get a list of all files starting with the supplied
   parent directory. Excluding the directories themselves and ANY 
   dotfiles.
-}
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents parent = do
   fullContents <- getDirectoryContents parent
   let contents = filter (not . isPrefixOf ".") fullContents
   paths <- forM contents $ \name -> do
      let path = parent </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
         then getRecursiveContents path
         else return [path]
   return $ concat paths


{- | Construct a zip Archive containing epub book data from the 
     specified directory
-}
mkEpubArchive :: FilePath -> IO Archive
mkEpubArchive rootDir = do
   setCurrentDirectory rootDir

   allFiles <- getRecursiveContents "."

   flip (addFilesToArchive []) ["mimetype"] >=>
      flip (addFilesToArchive [OptRecursive]) allFiles
      $ emptyArchive


-- | Read a zip Archive from disk
readArchive :: FilePath -> IO Archive
readArchive = fmap toArchive . B.readFile


-- | Write a zip Archive to disk using the specified filename
writeArchive :: FilePath -> Archive -> IO ()
writeArchive zipPath = (B.writeFile zipPath) . fromArchive
