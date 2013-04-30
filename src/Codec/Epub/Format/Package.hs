-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing OPF package data
module Codec.Epub.Format.Package
   ( formatPackage
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )

import Codec.Epub.Data.Package
{-
import Codec.Epub.Format.Guide
import Codec.Epub.Format.Manifest
import Codec.Epub.Format.Metadata
import Codec.Epub.Format.Spine
-}
import Codec.Epub.Format.Util


tellPackage :: MonadWriter (Seq Char) m => (String, String) -> m ()
tellPackage (version, uniqueId) = do
   tellSeq "package\n"
   tellSeq $ formatSubline "version" (Just version)
   tellSeq $ formatSubline "unique-identifier" (Just uniqueId)


formatPackage :: Bool -> Package -> String
formatPackage showAll (Package v u) =
   toList . execWriter $ do
      tellPackage (v, u)
{-
formatPackage :: Bool -> Package -> String
formatPackage showAll (Package v u meta ma sp gu) =
   toList . execWriter $ do
      tellPackage (v, u)
      tellMetadata meta

      when showAll $ do
         tellManifest ma
         tellSpine sp
         tellGuide gu
-}
