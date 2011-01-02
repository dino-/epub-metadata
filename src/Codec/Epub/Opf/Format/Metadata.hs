-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Opf.Format.Metadata
   where

import Control.Monad.Writer.Lazy
import Text.Printf

import Codec.Epub.Opf.Format.Util
import Codec.Epub.Opf.Package.Metadata


tellTitle :: MonadWriter (Seq Char) m => MetaTitle -> m ()
tellTitle (MetaTitle Nothing title) = tellSeq $ printf "title: %s\n" title
tellTitle (MetaTitle lang title) =
   tellSeq $ printf "title\n%s%s" (formatSubline "lang" lang)
      (formatSubline "title" (Just title))


tellCreator :: MonadWriter (Seq Char) m => MetaCreator -> m ()
tellCreator (MetaCreator Nothing Nothing creator) =
   tellSeq $ printf "creator: %s\n" creator
tellCreator (MetaCreator role fileAs creator) =
   tellSeq $ printf "creator\n%s%s%s"
      (formatSubline "role" role)
      (formatSubline "file-as" fileAs)
      (formatSubline "creator" (Just creator))


tellContributor :: MonadWriter (Seq Char) m => MetaCreator -> m ()
tellContributor (MetaCreator Nothing Nothing contributor) =
   tellSeq $ printf "contributor: %s\n" contributor
tellContributor (MetaCreator role fileAs contributor) =
   tellSeq $ printf "contributor\n%s%s%s"
      (formatSubline "role" role)
      (formatSubline "file-as" fileAs)
      (formatSubline "creator" (Just contributor))


tellDate :: MonadWriter (Seq Char) m => MetaDate -> m ()
tellDate (MetaDate Nothing date) =
   tellSeq $ printf "date: %s\n" date
tellDate (MetaDate event date) =
   tellSeq $ printf "date\n%s%s"
      (formatSubline "event" event)
      (formatSubline "date" (Just date))


tellType :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellType = maybe (return ()) (tellSeq . (printf "type: %s\n"))


tellFormat :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellFormat = maybe (return ()) (tellSeq . (printf "format: %s\n"))


tellId :: MonadWriter (Seq Char) m => MetaId -> m ()
tellId (MetaId idVal scheme content) =
   tellSeq $ printf "identifier\n%s%s%s"
      (formatSubline "id" (Just idVal))
      (formatSubline "scheme" scheme)
      (formatSubline "identifier" (Just content))


tellSource :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellSource = maybe (return ()) (tellSeq . (printf "source: %s\n"))


tellSubject :: MonadWriter (Seq Char) m => String -> m ()
tellSubject = tellSeq . (printf "subject: %s\n")


tellDescription :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellDescription = maybe (return ()) (tellSeq . (printf "description: %s\n"))


tellPublisher :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellPublisher = maybe (return ()) (tellSeq . (printf "publisher: %s\n"))


tellLang :: MonadWriter (Seq Char) m => String -> m ()
tellLang = tellSeq . (printf "language: %s\n")


tellRelation :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellRelation = maybe (return ()) (tellSeq . (printf "relation: %s\n"))


tellCoverage :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellCoverage = maybe (return ()) (tellSeq . (printf "coverage: %s\n"))


tellRights :: MonadWriter (Seq Char) m => Maybe String -> m ()
tellRights = maybe (return ()) (tellSeq . (printf "rights: %s\n"))


tellMetadata :: MonadWriter (Seq Char) m => Metadata -> m ()
tellMetadata meta = do
   mapM_ tellTitle $ metaTitles meta
   mapM_ tellCreator $ metaCreators meta
   mapM_ tellContributor $ metaContributors meta
   mapM_ tellDate $ metaDates meta
   tellType $ metaType meta
   tellFormat $ metaFormat meta
   mapM_ tellId $ metaIds meta
   tellSource $ metaSource meta
   mapM_ tellSubject $ metaSubjects meta
   tellDescription $ metaDescription meta
   tellPublisher $ metaPublisher meta
   mapM_ tellLang $ metaLangs meta
   tellRelation $ metaRelation meta
   tellCoverage $ metaCoverage meta
   tellRights $ metaRights meta
