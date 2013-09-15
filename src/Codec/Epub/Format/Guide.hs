-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- | Module for pretty-printing epub guide info

   Used internally by Codec.Epub.Format
-}
module Codec.Epub.Format.Guide
   ( formatGuide
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )
import Text.Printf

import Codec.Epub.Format.Util
import Codec.Epub.Data.Guide


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


{- | Format an epub Guide structure for pretty printing
-}
formatGuide :: [GuideRef] -> String
formatGuide guideRefs = toList . execWriter $ tellGuide guideRefs
