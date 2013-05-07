-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the metadata from an ePub file
module Codec.Epub.Parse
   ( getGuide
   , getManifest
   , getMetadata
   , getPackage
   , getSpine
   )
   where

import Control.Arrow.ListArrows
import Control.Monad.Error
import Text.XML.HXT.Arrow.Namespace ( propagateNamespaces )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.IO
import Codec.Epub.Data.Guide
import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Codec.Epub.Data.Spine
import Codec.Epub.Parse.Guide
import Codec.Epub.Parse.Manifest
import Codec.Epub.Parse.Metadata
import Codec.Epub.Parse.Package
import Codec.Epub.Parse.Spine


{- | Extract the ePub OPF Package data contained in the supplied 
   XML string
-}
performParse :: (MonadIO m, MonadError String m) =>
   IOSLA (XIOState ()) XmlTree b -> String -> m b
performParse parser contents = do
   {- Improper encoding and schema declarations have been causing
      havok with this parse, cruelly strip them out. -}
   let cleanedContents = removeIllegalStartChars . removeEncoding
         . removeDoctype $ contents
   
   result <- liftIO $ runX (
      readString [withValidate no] cleanedContents
      >>> propagateNamespaces
      >>> parser
      )

   case result of
      (r : []) -> return r
      _        -> throwError
         "ERROR: FIXME with a better message"


getGuide :: (MonadIO m, MonadError String m) =>
   String -> m [GuideRef]
getGuide = performParse guideP


getManifest :: (MonadIO m, MonadError String m) =>
   String -> m [ManifestItem]
getManifest = performParse manifestP


getMetadata :: (MonadIO m, MonadError String m) =>
   String -> m Metadata
getMetadata = performParse metadataP


getPackage :: (MonadIO m, MonadError String m) =>
   String -> m Package
getPackage = performParse packageP


getSpine :: (MonadIO m, MonadError String m) =>
   String -> m Spine
getSpine = performParse spineP
