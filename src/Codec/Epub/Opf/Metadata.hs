-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Codec.Epub.Opf.Metadata
   where

import Control.Monad.Error
import HSH.Command
import Text.HTML.TagSoup
import Text.Printf


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


opfPath :: (MonadError String m, MonadIO m) =>
   FilePath -> m String
opfPath zipPath = do
   containerContents <- extractFileFromZip zipPath
      "META-INF/container.xml"

   let containerTags = parseTags containerContents

   return $ fromAttrib "full-path" . head . head .
         sections (~== "<rootfile>") $ containerTags


extractTitle :: [Tag String] -> String
extractTitle = fromTagText . head . filter isTagText
   . head . (sections (~== "<dc:title>"))


extractCreator :: [Tag String] -> Creator
extractCreator tags = Creator role creator
   where
      creatorTags = head . (sections (~== "<dc:creator>")) $ tags

      role = fromAttrib "opf:role" . head $ creatorTags

      creator = fromTagText . head . filter isTagText
         $ creatorTags


extractDates :: [Tag String] -> [Date]
extractDates = map extractDate . sections (~== "<dc:date>")
   where
      extractDate ts = (event, date)
         where
            event = fromAttrib "opf:event" . head $ ts
            date = fromTagText . head . filter isTagText $ ts


extractEpubMeta :: (MonadIO m, MonadError String m) =>
   FilePath -> m EpubMeta
extractEpubMeta zipPath = EpubMeta
   `liftM` (extractTitle `liftM` opfTags)
   `ap`    (extractCreator `liftM` opfTags)
   `ap`    (extractDates `liftM` opfTags)

   where
      opfTags = do
         opfContents <- extractFileFromZip zipPath =<< opfPath zipPath

         return $ head . sections (~== "<metadata>") . parseTags
            $ opfContents


extractFileFromZip ::
   (MonadIO m, MonadError [Char] m) =>
   FilePath -> FilePath -> m String
extractFileFromZip zipPath filePath = do
   let app = "unzip"
   result <- liftIO $ tryEC $ run
      ((printf "%s -p %s %s" app zipPath filePath) :: String)
   case result of
      Left ps -> throwError $
         printf "[ERROR %s  zip file: %s  path in zip: %s  status: %s]"
            app zipPath filePath (show ps)
      Right output -> return output
