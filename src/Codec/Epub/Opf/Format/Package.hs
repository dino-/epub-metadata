-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing OPF package data
module Codec.Epub.Opf.Format.Package
   ( formatPackage
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )

import Codec.Epub.Opf.Format.Guide
import Codec.Epub.Opf.Format.Manifest
import Codec.Epub.Opf.Format.Metadata
import Codec.Epub.Opf.Format.Spine
import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package


tellPackage :: MonadWriter (Seq Char) m => (String, String) -> m ()
tellPackage (version, uniqueId) = do
   tellSeq "package\n"
   tellSeq $ formatSubline "version" (Just version)
   tellSeq $ formatSubline "unique-identifier" (Just uniqueId)


formatPackage :: Bool -> Package -> String
formatPackage showAll (Package v u meta ma sp gu) =
   toList . execWriter $ do
      tellPackage (v, u)
      tellMetadata meta

      when showAll $ do
         tellManifest ma
         tellSpine sp
         tellGuide gu
