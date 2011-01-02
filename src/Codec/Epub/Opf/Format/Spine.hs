-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Spine
   where

import Control.Monad.Writer.Lazy
import Text.Printf

import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package.Spine


tellSpineItemref :: MonadWriter (Seq Char) m => SpineItemref -> m ()
tellSpineItemref (SpineItemref idref linear) =
   tellSeq $ printf "   idref: %s%s\n" idref (linearToString linear)

   where
      boolToYn True  = "yes"
      boolToYn False = "no"

      linearToString Nothing = ""
      linearToString (Just l) = printf ", linear: %s" (boolToYn l)


tellSpine :: MonadWriter (Seq Char) m => Spine -> m ()
tellSpine (Spine toc itemRefs) = do
   tellSeq $ printf "spine toc: %s, itemrefs:\n" toc
   mapM_ tellSpineItemref itemRefs
