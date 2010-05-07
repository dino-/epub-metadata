-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Opf.Metadata.Format
   ( emToString
   )
   where

import Text.Printf

import Codec.Epub.Opf.Metadata


formatSubline :: String -> Maybe String -> String
formatSubline _   Nothing = ""
formatSubline key (Just value) = printf "   %s: %s\n" key value


titleToString :: EMTitle -> String
titleToString (EMTitle Nothing title) = "title: " ++ title
titleToString (EMTitle lang title) = init $
   "title\n" ++
   (formatSubline "lang" lang) ++
   (formatSubline "title" (Just title))


creatorToString :: EMCreator -> String
creatorToString (EMCreator Nothing Nothing creator) =
   "creator: " ++ creator
creatorToString (EMCreator role fileAs creator) = init $
   "creator\n" ++
   (formatSubline "role" role) ++
   (formatSubline "file-as" fileAs) ++
   (formatSubline "creator" (Just creator))


dateToString :: EMDate -> String
dateToString (EMDate Nothing date) =
   "date: " ++ date
dateToString (EMDate event date) = init $
   "date\n" ++
   (formatSubline "event" event) ++
   (formatSubline "date" (Just date))


emToString :: EpubMeta -> String
emToString em = unlines $
   (map titleToString $ emTitles em) ++
   (map creatorToString $ emCreators em) ++
   (map dateToString $ emDates em)
