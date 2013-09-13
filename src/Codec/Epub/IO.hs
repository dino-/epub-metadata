-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Functions for performing some IO operations on epub files

module Codec.Epub.IO
   ( getPkgXmlFromZip
   , getPkgXmlFromBS
   , getPkgXmlFromDir
   , mkEpubArchive
   , readArchive
   , writeArchive
   )
   where

import Codec.Archive.Zip
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Exception
import Control.Monad.Error
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ( fromChunks )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import System.Directory
import System.FilePath
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlArrow ( getAttrValue, hasName, isElem )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )


locateRootFile :: (MonadIO m, MonadError String m) =>
   FilePath -> String -> m FilePath
locateRootFile containerPath' containerDoc = do
   result <- liftIO $ runX (
      readString [withValidate no] containerDoc
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError $
         "ERROR: rootfile full-path missing from " ++ containerPath'


-- | Extract a file from a zip archive throwing an error on failure
fileFromArchive :: MonadError String m =>
   FilePath -> Archive -> m String
fileFromArchive filePath archive = do
   let mbEntry = findEntryByPath filePath archive
   maybe
      (throwError $ "Unable to locate file " ++ filePath)
      (return . BL.unpack . fromEntry) mbEntry


{- | The static location of the container.xml, as specified by the
     EPUB docs
-}
containerPath :: FilePath
containerPath = "META-INF/container.xml"


{- | Get the contents of the EPUB Package Document from a ByteString
     representing an EPUB (zip) file
-}
getPkgXmlFromBS :: (MonadError String m, MonadIO m)
   => BS.ByteString           -- ^ contents of the zip file
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the EPUB Package Document
getPkgXmlFromBS strictBytes = do
   -- Need to turn this strict byte string into a lazy one
   let lazyBytes = fromChunks [strictBytes]
   result <- liftIO $ ( try $ evaluate
      (toArchive lazyBytes) :: IO (Either SomeException Archive) )
   archive <- either (throwError . show) return result

   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   containerDoc <- fileFromArchive containerPath archive

   rootPath <- locateRootFile containerPath containerDoc

   -- Now that we have the path to the .opf file, extract it
   rootContents <- fileFromArchive rootPath archive

   return (rootPath, rootContents)


-- | Get the contents of the EPUB Package Document from an EPUB (zip) file
getPkgXmlFromZip :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ path to ePub zip file
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the EPUB Package Document
getPkgXmlFromZip zipPath = do
   {- Strictly read this file into a ByteString, send to 
      getPkgXmlFromBS
   -}
   zipFileBytes <- liftIO $ BS.readFile zipPath
   getPkgXmlFromBS zipFileBytes


{- | Get the contents of the EPUB Package Document from a directory
     containing the files from an EPUB file (as in: it's been
     unzipped into a dir)
-}
getPkgXmlFromDir :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ directory path
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the EPUB Package Document
getPkgXmlFromDir dir = do
   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   liftIO $ setCurrentDirectory dir

   containerDoc <- liftIO $ readFile containerPath

   rootPath <- locateRootFile (dir </> containerPath) containerDoc

   -- Now that we have the path to the .opf file, load it
   rootContents <- liftIO $ readFile rootPath

   return (rootPath, rootContents)


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

   let mimetype = ["mimetype"]
   allFiles <- getRecursiveContents "."
   let restFiles = allFiles \\ mimetype

   flip (addFilesToArchive [OptRecursive]) restFiles >=>
      flip (addFilesToArchive []) ["mimetype"]
      $ emptyArchive


-- | Read a zip Archive from disk
readArchive :: FilePath -> IO Archive
readArchive = fmap toArchive . B.readFile


-- | Write a zip Archive to disk using the specified filename
writeArchive :: FilePath -> Archive -> IO ()
writeArchive zipPath = (B.writeFile zipPath) . fromArchive
