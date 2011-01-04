-- Copyright: 2010, 2011 Dino Morelli
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

import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Monad.Error
import System.Exit
import System.Process
import Text.Printf
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


{- | GNU unzip has annoying non-zero exit codes that aren't fatal
   so we need to check for those special.
-}
handleEC :: (MonadIO m, MonadError String m)
   => String -> ExitCode -> m ()
handleEC msg (ExitFailure c)
   | c > 2 = throwError $ printf "%s  status: %s]\n" msg (show c)
   | otherwise = return ()
handleEC _    ExitSuccess = return ()


{- | Extract a file from a zipfile.
   This is here because ePub files are really just zip files.

   Yep, you saw right sports fans. This code is using the command-
   line unzip utility. In the future I'd like to make it use a
   library.
-}
extractFileFromZip :: (MonadIO m, MonadError String m)
   => FilePath    -- ^ path to zip file
   -> FilePath    -- ^ path within zip file to extract
   -> m String    -- ^ contents of expected file
extractFileFromZip zipPath filePath = do
   let dearchiver = "unzip"

   (ec, output, _) <- liftIO $ readProcessWithExitCode
      dearchiver ["-p", zipPath, filePath] ""

   handleEC (printf "[ERROR %s  zip file: %s  path in zip: %s"
      dearchiver zipPath filePath) ec

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
