-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows, FlexibleContexts #-}

{- | The main parsing interface, these get* functions are intended
   to be used by consumers of this library

   This module is called Parse because it invokes the XML parsing
   machinery of this library, but consumers of the library do not
   have to interact with HXT, Arrows or XML directly.
-}
module Codec.Epub.Parse
   ( getGuide
   , getManifest
   , getMetadata
   , getPackage
   , getSpine
   )
   where

import Control.Arrow.ListArrows
import Control.Monad.Except
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
import Codec.Epub.Util


{- Extract the epub OPF Package data contained in the supplied 
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


{- | Parse epub guide items from a String representing the epub XML
   Package Document
-}
getGuide :: (MonadIO m, MonadError String m) =>
   String -> m [GuideRef]
getGuide = performParse guideP


{- | Parse epub manifest data from a String representing the epub XML
   Package Document
-}
getManifest :: (MonadIO m, MonadError String m) =>
   String -> m Manifest
getManifest = performParse manifestP


{- | Parse epub metadata from a String representing the epub XML
   Package Document
-}
{- Parsing the metadata is a two-pass process
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


{- | Parse epub package info from a String representing the epub XML
   Package Document
-}
getPackage :: (MonadIO m, MonadError String m) =>
   String -> m Package
getPackage = performParse packageP


{- | Parse epub spine info from a String representing the epub XML
   Package Document
-}
getSpine :: (MonadIO m, MonadError String m) =>
   String -> m Spine
getSpine = performParse spineP
