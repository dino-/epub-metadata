-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Functions for doing some disk IO with ePub documents
-}
module Codec.Epub.IO
   ( opfContents )
   where

import Codec.Archive.Zip
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Monad.Error
import qualified Data.ByteString.Lazy.Char8 as B
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


-- | Extract a file from a zip archive throwing an error on failure
fileFromArchive :: MonadError String m =>
   FilePath -> Archive -> m String
fileFromArchive filePath archive = do
   let mbEntry = findEntryByPath filePath archive
   maybe
      (throwError $ "Unable to locate file " ++ filePath)
      (return . B.unpack . fromEntry) mbEntry


-- | Get the contents of the OPF Package Document from an ePub file
opfContents :: (MonadError String m, MonadIO m)
   => FilePath    -- ^ path to ePub zip file
   -> m String    -- ^ contents of the OPF Package Document
opfContents zipPath = do
   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   zipFileBytes <- liftIO $ B.readFile zipPath
   let archive = toArchive zipFileBytes

   let containerPath = "META-INF/container.xml"
   containerDoc <- fileFromArchive containerPath archive

   result <- liftIO $ runX (
      readString [withValidate no] containerDoc
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   rootPath <- case result of
      (p : []) -> return p
      _        -> throwError $
         "ERROR: rootfile full-path missing from " ++ containerPath

   -- Now that we have the path to the .opf file, extract it
   rootDoc <- fileFromArchive rootPath archive

   return . removeEncoding . removeDoctype $ rootDoc
