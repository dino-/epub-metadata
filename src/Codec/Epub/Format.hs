-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing OPF package data
module Codec.Epub.Format
   ( Formattable (..)
   )
   where

import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Codec.Epub.Data.Spine
import Codec.Epub.Format.Manifest
import Codec.Epub.Format.Metadata
import Codec.Epub.Format.Package
import Codec.Epub.Format.Spine


class Formattable a where
   format :: a -> String


instance Formattable Manifest where
   format = formatManifest

instance Formattable Metadata where
   format = formatMetadata

instance Formattable Package where
   format = formatPackage

instance Formattable Spine where
   format = formatSpine
