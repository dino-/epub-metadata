-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows, FlexibleContexts #-}

-- | Module for extracting the metadata from an epub file
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
import Text.Regex
import Text.XML.HXT.Arrow.Namespace ( propagateNamespaces )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.Data.Guide
import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Codec.Epub.Data.Spine
import Codec.Epub.Parse.Guide
import Codec.Epub.Parse.Manifest
import Codec.Epub.Parse.Metadata
import Codec.Epub.Parse.Package
import Codec.Epub.Parse.Refinements
import Codec.Epub.Parse.Spine


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


-- | An evil hack to remove any \<!DOCTYPE ...\> from the document
removeDoctype :: String -> String
removeDoctype = flip (subRegex 
   (mkRegexWithOpts "<!DOCTYPE [^>]*>" False True)) ""


{- | Extract the epub OPF Package data contained in the supplied 
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
   String -> m Manifest
getManifest = performParse manifestP


{- | Parsing the metadata is a two-pass process
   First we need to parse the meta tags only, referred to in this
   code as 'refinements.'
   Second we parse the metadata tags themselves, passing in the
   refinements so their info can be merged during parse
-}
getMetadata :: (MonadIO m, MonadError String m) =>
   String -> m Metadata
getMetadata opfContents = do
   refinements <- performParse refinementsP opfContents
   rawMd <- performParse (metadataP refinements) opfContents
   return rawMd


getPackage :: (MonadIO m, MonadError String m) =>
   String -> m Package
getPackage = performParse packageP


getSpine :: (MonadIO m, MonadError String m) =>
   String -> m Spine
getSpine = performParse spineP
