-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Package
   ( opfToString
   )
   where

import Text.Printf

import Codec.Epub.Opf.Format.Metadata
import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package


packageToString :: (String, String) -> String
packageToString (version, uniqueId) =
   "package\n" ++
   (formatSubline "version" (Just version)) ++
   (formatSubline "unique-identifier" (Just uniqueId))


-- | Format an ePub metadata into a String
opfToString :: OPFPackage -> String
opfToString (OPFPackage v u em ma sp gu) = concat $
   [packageToString (v, u)] ++
   (map titleToString $ emTitles em) ++
   (map creatorToString $ emCreators em) ++
   (map contributorToString $ emContributors em) ++
   (map dateToString $ emDates em) ++
   [typeToString . emType $ em] ++
   [formatToString . emFormat $ em] ++
   (map idToString $ emIds em) ++
   [sourceToString . emSource $ em] ++
   (map subjectToString $ emSubjects em) ++
   [descriptionToString . emDescription $ em] ++
   [publisherToString . emPublisher $ em] ++
   (map langToString $ emLangs em) ++
   [relationToString . emRelation $ em] ++
   [coverageToString . emCoverage $ em] ++
   [rightsToString . emRights $ em] ++
   ["manifest items:\n"] ++
   [unlines $ map (printf "    %s" . show) ma] ++
   [printf "spine idref=%s; items:\n" (show $ esID sp)] ++
   [unlines $ map (printf "    %s" . show) (esItemrefs sp)] ++
   [unlines $ map show gu]
