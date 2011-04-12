-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Functions for doing some disk IO with ePub documents
-}
module Codec.Epub.IO
   ( opfContentsFromZip
   , opfContentsFromBS
   , opfContentsFromDir
   , removeEncoding
   , removeDoctype
   )
   where

import Codec.Archive.Zip
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Monad.Error
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory
import System.FilePath
import Text.Regex
import Text.XML.HXT.Arrow.XmlArrow ( getAttrValue, hasName, isElem )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )
import Text.XML.HXT.Arrow.ReadDocument ( readString )


-- | An evil hack to remove encoding from the document
removeEncoding :: String -> String
removeEncoding = flip (subRegex 
   (mkRegexWithOpts " +encoding=\"UTF-8\"" False True)) ""


-- | An evil hack to remove any <!DOCTYPE ...> from the document
removeDoctype :: String -> String
removeDoctype = flip (subRegex 
   (mkRegexWithOpts "<!DOCTYPE [^>]*>" False True)) ""


locateRootFile :: (MonadIO m, MonadError String m) =>
   FilePath -> String -> m FilePath
locateRootFile containerPath containerDoc = do
   result <- liftIO $ runX (
      readString [withValidate no] containerDoc
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError $
         "ERROR: rootfile full-path missing from " ++ containerPath


-- | Extract a file from a zip archive throwing an error on failure
fileFromArchive :: MonadError String m =>
   FilePath -> Archive -> m String
fileFromArchive filePath archive = do
   let mbEntry = findEntryByPath filePath archive
   maybe
      (throwError $ "Unable to locate file " ++ filePath)
      (return . B.unpack . fromEntry) mbEntry


-- | Get the contents of the OPF Package Document from a ByteString
opfContentsFromBS :: (MonadError String m, MonadIO m)
   => B.ByteString            -- ^ contents of the zip file
   -> m (FilePath, String)    -- ^ path and contents of the OPF Package Document
opfContentsFromBS bytes = do
   let archive = toArchive bytes

   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   let containerPath = "META-INF/container.xml"
   containerDoc <- fileFromArchive containerPath archive

   rootPath <- locateRootFile containerPath containerDoc

   -- Now that we have the path to the .opf file, extract it
   rootContents <- fileFromArchive rootPath archive

   return (rootPath, rootContents)


-- | Get the contents of the OPF Package Document from an ePub file
opfContentsFromZip :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ path to ePub zip file
   -> m (FilePath, String)    -- ^ path and contents of the OPF Package Document
opfContentsFromZip zipPath = do
   {- Lazily read this file into a ByteString, send to 
      opfContentsFromBS
   -}
   zipFileBytes <- liftIO $ B.readFile zipPath
   opfContentsFromBS zipFileBytes


-- | Get the contents of the OPF Package Document from an ePub file
opfContentsFromDir :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ directory path
   -> m (FilePath, String)    -- ^ path and contents of the OPF Package Document
opfContentsFromDir dir = do
   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   liftIO $ setCurrentDirectory dir

   let containerPath = "META-INF/container.xml"
   containerDoc <- liftIO $ readFile containerPath

   rootPath <- locateRootFile (dir </> containerPath) containerDoc

   -- Now that we have the path to the .opf file, load it
   rootContents <- liftIO $ readFile rootPath

   return (rootPath, rootContents)
