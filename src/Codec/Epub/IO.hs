-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Functions for doing some disk IO with ePub documents
-}
module Codec.Epub.IO
   ( extractFileFromZip, opfPath )
   where

import Codec.Archive.LibZip
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Monad.Error
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


{- | Extract a file from a zipfile.
-}
extractFileFromZip :: (MonadIO m, MonadError String m)
   => FilePath    -- ^ path to zip file
   -> FilePath    -- ^ path within zip file to extract
   -> m String    -- ^ contents of expected file
extractFileFromZip zipPath filePath = do
   result <- liftIO $ catchZipError
      (fmap Right $ withArchive [] zipPath $ fileContents [] filePath)
      (return . Left)

   output <- either (throwError . show) return result

   return . removeEncoding . removeDoctype $ output


-- | Get the path within an ePub file to the OPF Package Document
opfPath :: (MonadError String m, MonadIO m)
   => FilePath    -- ^ path to ePub zip file
   -> m String    -- ^ path within ePub to the OPF Package Document
opfPath zipPath = do
   containerContents <- extractFileFromZip zipPath
      "META-INF/container.xml"

   result <- liftIO $ runX (
      readString [withValidate no] containerContents
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError
         "ERROR: rootfile full-path missing from META-INF/container.xml"
