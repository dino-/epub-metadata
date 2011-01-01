-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Functions shared by several formatting modules
module Codec.Epub.Opf.Format.Util
   ( formatSubline
   )
   where

import Text.Printf


formatSubline :: String -> Maybe String -> String
formatSubline _   Nothing = ""
formatSubline key (Just value) = printf "   %s: %s\n" key value
