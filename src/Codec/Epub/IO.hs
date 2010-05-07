-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Codec.Epub.IO
   ( extractFileFromZip, opfPath )
   where

import Control.Monad.Error
import HSH.Command
import Text.Printf
import Text.XML.HXT.Arrow


extractFileFromZip ::
   (MonadIO m, MonadError [Char] m) =>
   FilePath -> FilePath -> m String
extractFileFromZip zipPath filePath = do
   let dearchiver = "unzip"
   result <- liftIO $ tryEC $ run
      ((printf "%s -p %s %s" dearchiver zipPath filePath) :: String)
   case result of
      Left ps -> throwError $
         printf "[ERROR %s  zip file: %s  path in zip: %s  status: %s]"
            dearchiver zipPath filePath (show ps)
      Right output -> return output


opfPath :: (MonadError String m, MonadIO m) =>
   FilePath -> m String
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
