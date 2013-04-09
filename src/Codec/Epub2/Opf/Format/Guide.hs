-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing ePub metadata info
module Codec.Epub2.Opf.Format.Guide
   where

import Control.Monad.Writer.Lazy
import Text.Printf

import Codec.Epub2.Opf.Format.Util
import Codec.Epub2.Opf.Package.Guide


tellGuideRef :: MonadWriter (Seq Char) m => GuideRef -> m ()
tellGuideRef (GuideRef grty title href) =
   tellSeq $ printf "   type: %s%s, href: %s\n"
      grty (titleToString title) href

   where
      titleToString = maybe "" (printf ", title: %s")


tellGuide :: MonadWriter (Seq Char) m => [GuideRef] -> m ()
tellGuide []  = return ()
tellGuide grs = do
   tellSeq "guide items:\n"
   mapM_ tellGuideRef grs
