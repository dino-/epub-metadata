-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Functions for doing some disk IO with ePub documents

   Note that these functions do their work by using the external 
   unzip utility.
-}
module Codec.Epub.IO
   ( extractFileFromZip, opfPath )
   where

import Control.Monad.Error
import HSH.Command
import Text.Printf
import Text.XML.HXT.Arrow


{- | Extract a file from a zipfile.
   This is here because ePub files are really just zip files.
-}
extractFileFromZip :: (MonadIO m, MonadError [Char] m)
   => FilePath    -- ^ path to zip file
   -> FilePath    -- ^ path within zip file to extract
   -> m String    -- ^ contents of expected file
extractFileFromZip zipPath filePath = do
   let dearchiver = "unzip"
   result <- liftIO $ tryEC $ run
      ((printf "%s -p %s %s" dearchiver zipPath filePath) :: String)
   case result of
      Left ps -> throwError $
         printf "[ERROR %s  zip file: %s  path in zip: %s  status: %s]"
            dearchiver zipPath filePath (show ps)
      Right output -> return output


-- | Get the path within an ePub file to the OPF Package Document
opfPath :: (MonadError String m, MonadIO m)
   => FilePath    -- ^ path to ePub zip file
   -> m String    -- ^ path within ePub to the OPF Package Document
opfPath zipPath = do
   containerContents <- extractFileFromZip zipPath
      "META-INF/container.xml"

   result <- liftIO $ runX (
      readString [(a_validate, v_0)] containerContents
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError
         "ERROR: rootfile full-path missing from META-INF/container.xml"
