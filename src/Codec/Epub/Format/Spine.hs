-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Module for pretty-printing epub spine info

   Used internally by Codec.Epub.Format
-}
module Codec.Epub.Format.Spine
   ( formatSpine
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )
import Text.Printf

import Codec.Epub.Format.Util
import Codec.Epub.Data.Spine


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


{- | Format an epub Spine structure for pretty printing
-}
formatSpine :: Spine -> String
formatSpine sp = toList . execWriter
   $ tellSpine sp
