-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions shared by several formatting modules
module Codec.Epub.Format.Util
   ( formatSubline
   , tellSeq
   , Seq
   )
   where

import Control.Monad.Writer.Lazy
import Data.Sequence ( Seq, fromList )
import Text.Printf


formatSubline :: String -> Maybe String -> String
formatSubline _   Nothing = ""
formatSubline key (Just value) = printf "   %s: %s\n" key value


tellSeq :: MonadWriter (Seq a) m => [a] -> m ()
tellSeq = tell . fromList
