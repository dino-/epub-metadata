-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Metadata
   where

import Text.Printf

import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package.Metadata


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


subjectToString :: String -> String
subjectToString = printf "subject: %s\n"


descriptionToString :: Maybe String -> String
descriptionToString = maybe "" (printf "description: %s\n")


publisherToString :: Maybe String -> String
publisherToString = maybe "" (printf "publisher: %s\n")


dateToString :: EMDate -> String
dateToString (EMDate Nothing date) =
   printf "date: %s\n" date
dateToString (EMDate event date) =
   "date\n" ++
   (formatSubline "event" event) ++
   (formatSubline "date" (Just date))


typeToString :: Maybe String -> String
typeToString = maybe "" (printf "type: %s\n")


formatToString :: Maybe String -> String
formatToString = maybe "" (printf "format: %s\n")


idToString :: EMId -> String
idToString (EMId idVal scheme content) =
   "identifier\n" ++
   (formatSubline "id" (Just idVal)) ++
   (formatSubline "scheme" scheme) ++
   (formatSubline "identifier" (Just content))


sourceToString :: Maybe String -> String
sourceToString = maybe "" (printf "source: %s\n")


langToString :: String -> String
langToString = printf "language: %s\n"


relationToString :: Maybe String -> String
relationToString = maybe "" (printf "relation: %s\n")


coverageToString :: Maybe String -> String
coverageToString = maybe "" (printf "coverage: %s\n")


rightsToString :: Maybe String -> String
rightsToString = maybe "" (printf "rights: %s\n")
