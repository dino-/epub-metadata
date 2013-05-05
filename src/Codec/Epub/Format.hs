-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing OPF package data
module Codec.Epub.Format
   ( formatManifest
   , formatMetadata
   , formatPackage
   )
   where

import Codec.Epub.Format.Manifest
import Codec.Epub.Format.Metadata
import Codec.Epub.Format.Package
