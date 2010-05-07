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
titleToString (EMTitle Nothing title) = printf "title: %s\n" title
titleToString (EMTitle lang title) =
   "title\n" ++
   (formatSubline "lang" lang) ++
   (formatSubline "title" (Just title))


creatorToString :: EMCreator -> String
creatorToString (EMCreator Nothing Nothing creator) =
   printf "creator: %s\n" creator
creatorToString (EMCreator role fileAs creator) =
   "creator\n" ++
   (formatSubline "role" role) ++
   (formatSubline "file-as" fileAs) ++
   (formatSubline "creator" (Just creator))


contributorToString :: EMCreator -> String
contributorToString (EMCreator Nothing Nothing contributor) =
   printf "contributor: %s\n" contributor
contributorToString (EMCreator role fileAs contributor) =
   "contributor\n" ++
   (formatSubline "role" role) ++
   (formatSubline "file-as" fileAs) ++
   (formatSubline "creator" (Just contributor))


publisherToString :: Maybe String -> String
publisherToString = maybe "" (printf "publisher: %s\n")


dateToString :: EMDate -> String
dateToString (EMDate Nothing date) =
   printf "date: %s\n" date
dateToString (EMDate event date) =
   "date\n" ++
   (formatSubline "event" event) ++
   (formatSubline "date" (Just date))


idToString :: EMId -> String
idToString (EMId idVal scheme content) =
   "identifier\n" ++
   (formatSubline "id" (Just idVal)) ++
   (formatSubline "scheme" scheme) ++
   (formatSubline "identifier" (Just content))


langToString :: String -> String
langToString = printf "language: %s\n"


emToString :: EpubMeta -> String
emToString em = concat $
   (map titleToString $ emTitles em) ++
   (map creatorToString $ emCreators em) ++
   (map contributorToString $ emContributors em) ++
   [publisherToString . emPublisher $ em] ++
   (map dateToString $ emDates em) ++
   (map idToString $ emIds em) ++
   (map langToString $ emLangs em)
