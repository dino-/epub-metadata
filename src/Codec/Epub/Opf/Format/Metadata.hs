-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Metadata
   where

import Text.Printf

import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package.Metadata


titleToString :: MetaTitle -> String
titleToString (MetaTitle Nothing title) = printf "title: %s\n" title
titleToString (MetaTitle lang title) =
   "title\n" ++
   (formatSubline "lang" lang) ++
   (formatSubline "title" (Just title))


creatorToString :: MetaCreator -> String
creatorToString (MetaCreator Nothing Nothing creator) =
   printf "creator: %s\n" creator
creatorToString (MetaCreator role fileAs creator) =
   "creator\n" ++
   (formatSubline "role" role) ++
   (formatSubline "file-as" fileAs) ++
   (formatSubline "creator" (Just creator))


contributorToString :: MetaCreator -> String
contributorToString (MetaCreator Nothing Nothing contributor) =
   printf "contributor: %s\n" contributor
contributorToString (MetaCreator role fileAs contributor) =
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


dateToString :: MetaDate -> String
dateToString (MetaDate Nothing date) =
   printf "date: %s\n" date
dateToString (MetaDate event date) =
   "date\n" ++
   (formatSubline "event" event) ++
   (formatSubline "date" (Just date))


typeToString :: Maybe String -> String
typeToString = maybe "" (printf "type: %s\n")


formatToString :: Maybe String -> String
formatToString = maybe "" (printf "format: %s\n")


idToString :: MetaId -> String
idToString (MetaId idVal scheme content) =
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
