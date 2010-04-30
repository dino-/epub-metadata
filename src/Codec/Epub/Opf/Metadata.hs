-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Opf.Metadata
   where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe ( fromJust )
import Text.HTML.TagSoup


data Creator = Creator
   String   -- role
   String   -- creator
   deriving Show

type Date =
   ( String    -- event
   , String    -- date value
   )

data EpubMeta = EpubMeta
   { emTitle :: String
--   , emLanguage :: String
--   , emIdentifier :: String
   , emCreator :: Creator
--   , emContributor :: String
--   , emDescription :: String
   , emDates :: [Date]
   }
   deriving Show


opfPath :: Archive -> B.ByteString
opfPath archive =
   fromAttrib (B.pack "full-path") . head . head
      . sections (~== "<rootfile>") $ containerTags archive
   where
      containerTags = parseTags . fromEntry . fromJust
         . findEntryByPath "META-INF/container.xml"


extractTitle :: [Tag B.ByteString] -> String
extractTitle = B.unpack . fromTagText . head . filter isTagText
   . head . (sections (~== "<dc:title>"))


extractCreator :: [Tag B.ByteString] -> Creator
extractCreator tags = Creator role creator
   where
      creatorTags = head . (sections (~== "<dc:creator>")) $ tags

      role = B.unpack . fromAttrib (B.pack "opf:role")
         . head $ creatorTags

      creator = B.unpack . fromTagText . head . filter isTagText
         $ creatorTags


extractDates :: [Tag B.ByteString] -> [Date]
extractDates = map extractDate . sections (~== "<dc:date>")
   where
      extractDate ts = (event, date)
         where
            event = B.unpack . fromAttrib (B.pack "opf:event") . head $ ts
            date = B.unpack . fromTagText . head . filter isTagText $ ts


extractEpubMeta :: Archive -> EpubMeta
--extractEpubMeta archive = EpubMeta $ extractTitle opfTags
--extractEpubMeta archive = opfTags
extractEpubMeta archive = EpubMeta
   (extractTitle opfTags)
   (extractCreator opfTags)
   (extractDates opfTags)

   where
      opfTags = head . sections (~== "<metadata>") . parseTags
         . fromEntry . fromJust
         . (findEntryByPath (B.unpack $ opfPath archive))
         $ archive
