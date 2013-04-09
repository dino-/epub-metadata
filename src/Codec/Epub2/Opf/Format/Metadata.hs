-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing ePub metadata info
module Codec.Epub2.Opf.Format.Metadata
   where

import Control.Monad.Writer.Lazy
import Text.Printf

import Codec.Epub2.Opf.Format.Util
import Codec.Epub2.Opf.Package.Metadata


tellTitle :: MonadWriter (Seq Char) m => Title -> m ()
tellTitle (Title Nothing title) = tellSeq $ printf "title: %s\n" title
tellTitle (Title lang title) =
   tellSeq $ printf "title\n%s%s" (formatSubline "lang" lang)
      (formatSubline "text" (Just title))


tellCreator :: MonadWriter (Seq Char) m => Creator -> m ()
tellCreator (Creator Nothing Nothing creator) =
   tellSeq $ printf "creator: %s\n" creator
tellCreator (Creator role fileAs creator) =
   tellSeq $ printf "creator\n%s%s%s"
      (formatSubline "role" role)
      (formatSubline "file-as" fileAs)
      (formatSubline "text" (Just creator))


tellContributor :: MonadWriter (Seq Char) m => Creator -> m ()
tellContributor (Creator Nothing Nothing contributor) =
   tellSeq $ printf "contributor: %s\n" contributor
tellContributor (Creator role fileAs contributor) =
   tellSeq $ printf "contributor\n%s%s%s"
      (formatSubline "role" role)
      (formatSubline "file-as" fileAs)
      (formatSubline "text" (Just contributor))


tellDate :: MonadWriter (Seq Char) m => Date -> m ()
tellDate (Date Nothing date) =
   tellSeq $ printf "date: %s\n" date
tellDate (Date event date) =
   tellSeq $ printf "date\n%s%s"
      (formatSubline "event" event)
      (formatSubline "text" (Just date))


tellId :: MonadWriter (Seq Char) m => Identifier -> m ()
tellId (Identifier idVal scheme content) =
   tellSeq $ printf "identifier\n%s%s%s"
      (formatSubline "id" (Just idVal))
      (formatSubline "scheme" scheme)
      (formatSubline "text" (Just content))


tellDescription :: MonadWriter (Seq Char) m => Description -> m ()
tellDescription (Description Nothing text) =
   tellSeq $ printf "description: %s\n" text
tellDescription (Description lang text) =
   tellSeq $ printf "description\n%s%s"
      (formatSubline "lang" lang)
      (formatSubline "text" (Just text))


tellSimpleString :: MonadWriter (Seq Char) m => String -> String -> m ()
tellSimpleString label = tellSeq . (printf "%s: %s\n" label)


tellMetadata :: MonadWriter (Seq Char) m => Metadata -> m ()
tellMetadata (Metadata titles creators contributors subjects desc 
      publisher dates mType format ids source langs relation 
      coverage rights) = do
   mapM_ tellTitle titles
   mapM_ tellDescription desc
   mapM_ tellDate dates
   mapM_ tellCreator creators
   mapM_ tellContributor contributors
   mapM_ (tellSimpleString "publisher") publisher
   mapM_ tellId ids
   mapM_ (tellSimpleString "language") langs
   mapM_ (tellSimpleString "subject") subjects
   mapM_ (tellSimpleString "type") mType
   mapM_ (tellSimpleString "format") format
   mapM_ (tellSimpleString "source") source
   mapM_ (tellSimpleString "relation") relation
   mapM_ (tellSimpleString "coverage") coverage
   mapM_ (tellSimpleString "rights") rights
