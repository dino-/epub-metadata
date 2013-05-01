-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Functions for doing some disk IO with ePub documents

module Codec.Epub.IO
   ( getPkgXmlFromZip
   , getPkgXmlFromBS
   , getPkgXmlFromDir
   , removeIllegalStartChars
   , removeEncoding
   , removeDoctype
   )
   where

import Codec.Archive.Zip
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Exception
import Control.Monad.Error
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ( fromChunks )
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory
import System.FilePath
import Text.Regex
import Text.XML.HXT.Arrow.XmlArrow ( getAttrValue, hasName, isElem )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )
import Text.XML.HXT.Arrow.ReadDocument ( readString )


{- | An evil hack to remove *ILLEGAL* characters before the XML
     declaration. Why do people write software that does this?
     Can't they follow directions?
-}
removeIllegalStartChars :: String -> String
removeIllegalStartChars = dropWhile (/= '<')


-- | An evil hack to remove encoding from the document
removeEncoding :: String -> String
removeEncoding = flip (subRegex 
   (mkRegexWithOpts " +encoding=\"UTF-8\"" False False)) ""


-- | An evil hack to remove any <!DOCTYPE ...> from the document
removeDoctype :: String -> String
removeDoctype = flip (subRegex 
   (mkRegexWithOpts "<!DOCTYPE [^>]*>" False True)) ""


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
   -> m (FilePath, String)    -- ^ path and contents of the EPUB Package Document
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
   -> m (FilePath, String)    -- ^ path and contents of the EPUB Package Document
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
   -> m (FilePath, String)    -- ^ path and contents of the EPUB Package Document
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
