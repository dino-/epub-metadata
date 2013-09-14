-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing epub metadata info
module Codec.Epub.Format.Manifest
   ( formatManifest
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )
import Text.Printf

import Codec.Epub.Format.Util
import Codec.Epub.Data.Manifest


tellManifestItem :: MonadWriter (Seq Char) m => ManifestItem -> m ()
tellManifestItem (ManifestItem mfId href mediaType) =
   tellSeq $ printf "   id: %s, href: %s, media-type: %s\n"
      mfId href mediaType


tellManifest :: MonadWriter (Seq Char) m => [ManifestItem] -> m ()
tellManifest mas = do
   tellSeq "manifest items:\n"
   mapM_ tellManifestItem mas


formatManifest :: Manifest -> String
formatManifest (Manifest mis) = toList . execWriter
   $ tellManifest mis
