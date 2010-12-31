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
opfToString (OPFPackage v u meta ma sp gu) = concat $
   [packageToString (v, u)] ++
   (map titleToString $ metaTitles meta) ++
   (map creatorToString $ metaCreators meta) ++
   (map contributorToString $ metaContributors meta) ++
   (map dateToString $ metaDates meta) ++
   [typeToString . metaType $ meta] ++
   [formatToString . metaFormat $ meta] ++
   (map idToString $ metaIds meta) ++
   [sourceToString . metaSource $ meta] ++
   (map subjectToString $ metaSubjects meta) ++
   [descriptionToString . metaDescription $ meta] ++
   [publisherToString . metaPublisher $ meta] ++
   (map langToString $ metaLangs meta) ++
   [relationToString . metaRelation $ meta] ++
   [coverageToString . metaCoverage $ meta] ++
   [rightsToString . metaRights $ meta] ++
   ["manifest items:\n"] ++
   [unlines $ map (printf "    %s" . show) ma] ++
   [printf "spine idref=%s; items:\n" (show $ esID sp)] ++
   [unlines $ map (printf "    %s" . show) (esItemrefs sp)] ++
   [unlines $ map show gu]
