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
import Codec.Epub.Format.Util


tellPackage :: MonadWriter (Seq Char) m => (String, String) -> m ()
tellPackage (version, uniqueId) = do
   tellSeq "package\n"
   tellSeq $ formatSubline "version" (Just version)
   tellSeq $ formatSubline "unique-identifier" (Just uniqueId)


formatPackage :: Package -> String
formatPackage (Package v u) = toList . execWriter
   $ tellPackage (v, u)
