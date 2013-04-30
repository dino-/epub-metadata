-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for extracting the metadata from an ePub file
module Codec.Epub.Parse.Common
   ( atQTag
   , dcName
   , mbGetAttrValue
   , mbGetQAttrValue
   , notNullA
   , opfName
   , performParse
   , text
   , xmlName
   )
   where

--import Control.Applicative
import Control.Arrow.ListArrows
import Control.Monad.Error
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.Namespace ( propagateNamespaces )
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.DOM.TypeDefs

import Codec.Epub.IO
--import Codec.Epub.Data.Package


-- HXT helpers

{- Not used at this time. But may be used someday

atTag :: (ArrowXml a) => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)
-}

atQTag :: (ArrowXml a) => QName -> a (NTree XNode) XmlTree
atQTag tag = deep (isElem >>> hasQName tag)

text :: (ArrowXml a) => a (NTree XNode) String
text = getChildren >>> getText

notNullA :: (ArrowList a) => a [b] [b]
notNullA = isA $ not . null


{- Not used at this time, we don't have any single but optional
   tags any longer

mbQTagText :: (ArrowXml a) => QName -> a (NTree XNode) (Maybe String)
mbQTagText tag =
   ( atQTag tag >>>
     text >>> notNullA >>> arr Just )
   `orElse`
   (constA Nothing)
-}


mbGetAttrValue :: (ArrowXml a) =>
   String -> a XmlTree (Maybe String)
mbGetAttrValue n =
   (getAttrValue n >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)

mbGetQAttrValue :: (ArrowXml a) =>
   QName -> a XmlTree (Maybe String)
mbGetQAttrValue qn =
   (getQAttrValue qn >>> notNullA >>> arr Just)
   `orElse` (constA Nothing)


{- ePub parsing helpers

   Note that these URIs could conceivably change in the future
   Is it ok that they're hardcoded like this?

   Well, ok, the xml namespace URI will probably never change.
-}

dcName, opfName, xmlName :: String -> QName
dcName local = mkQName "dc" local "http://purl.org/dc/elements/1.1/"
opfName local = mkQName "opf" local "http://www.idpf.org/2007/opf"
xmlName local = mkQName "xml" local "http://www.w3.org/XML/1998/namespace"


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


-- | Given the path to an ePub file, extract the OPF Package data
{-
parseEpub2Opf :: (MonadIO m, MonadError String m) =>
   FilePath -> m Package
parseEpub2Opf zipPath = do
   (_, contents) <- opfContentsFromZip zipPath
   parseXmlToOpf contents
-}
