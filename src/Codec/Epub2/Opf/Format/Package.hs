-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing OPF package data
module Codec.Epub2.Opf.Format.Package
   ( formatPackage
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )

import Codec.Epub2.Opf.Format.Guide
import Codec.Epub2.Opf.Format.Manifest
import Codec.Epub2.Opf.Format.Metadata
import Codec.Epub2.Opf.Format.Spine
import Codec.Epub2.Opf.Format.Util
import Codec.Epub2.Opf.Package


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
