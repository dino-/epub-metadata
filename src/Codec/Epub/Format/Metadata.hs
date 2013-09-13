-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

-- | Module for pretty-printing ePub metadata info
module Codec.Epub.Format.Metadata
   ( formatMetadata
   )
   where

import Control.Monad.Writer.Lazy
import Data.Foldable ( toList )
import Text.Printf

import Codec.Epub.Format.Util
import Codec.Epub.Data.Metadata


tellTitle :: MonadWriter (Seq Char) m => Title -> m ()
tellTitle title =
   tellSeq $ printf "title\n%s%s%s%s"
      (formatSubline "text" (Just $ titleText title))
      (formatSubline "lang" (titleLang title))
      (formatSubline "title-type" (Just $ titleType title))
      (formatSubline "display-seq" (show `fmap` titleSeq title))


tellCreator :: MonadWriter (Seq Char) m => Creator -> m ()
tellCreator (Creator Nothing Nothing Nothing creator) =
   tellSeq $ printf "creator: %s\n" creator
tellCreator (Creator role fileAs dseq creator) =
   tellSeq $ printf "creator\n%s%s%s%s"
      (formatSubline "text" (Just creator))
      (formatSubline "file-as" fileAs)
      (formatSubline "role" role)
      (formatSubline "display-seq" (show `fmap` dseq))


tellContributor :: MonadWriter (Seq Char) m => Creator -> m ()
tellContributor (Creator Nothing Nothing Nothing contributor) =
   tellSeq $ printf "contributor: %s\n" contributor
tellContributor (Creator role fileAs dseq contributor) =
   tellSeq $ printf "contributor\n%s%s%s%s"
      (formatSubline "text" (Just contributor))
      (formatSubline "file-as" fileAs)
      (formatSubline "role" role)
      (formatSubline "display-seq" (show `fmap` dseq))


tellDate :: MonadWriter (Seq Char) m => Date -> m ()
tellDate (Date Nothing date) =
   tellSeq $ printf "date: %s\n" date
tellDate (Date event date) =
   tellSeq $ printf "date\n%s%s"
      (formatSubline "event" event)
      (formatSubline "text" (Just date))


tellId :: MonadWriter (Seq Char) m => Identifier -> m ()
tellId ident =
   tellSeq $ printf "identifier\n%s%s%s%s"
      (formatSubline "id" (idId ident))
      (formatSubline "identifier-type" (idType ident))
      (formatSubline "scheme" (idScheme ident))
      (formatSubline "text" (Just . idText $ ident))


tellDescription :: MonadWriter (Seq Char) m => Description -> m ()
tellDescription (Description Nothing text) =
   tellSeq $ printf "description: %s\n" text
tellDescription (Description lang text) =
   tellSeq $ printf "description\n%s%s"
      (formatSubline "lang" lang)
      (formatSubline "text" (Just text))


tellSimpleString :: MonadWriter (Seq Char) m => String -> String -> m ()
tellSimpleString label = tellSeq . (printf "%s: %s\n" label)


tellSimpleMbString :: MonadWriter (Seq Char) m => String
   -> Maybe String -> m ()
tellSimpleMbString _     Nothing  = return ()
tellSimpleMbString label (Just s) = tellSimpleString label s


tellMetadata :: MonadWriter (Seq Char) m => Metadata -> m ()
tellMetadata (Metadata ids titles langs contributors creators dates modified source mType coverage desc format publisher relation rights subjects) = do
   mapM_ tellTitle titles
   mapM_ tellDescription desc
   mapM_ tellDate dates
   tellSimpleMbString "modified" modified
   mapM_ tellCreator creators
   mapM_ tellContributor contributors
   mapM_ (tellSimpleString "publisher") publisher
   mapM_ tellId ids
   mapM_ (tellSimpleString "language") langs
   mapM_ (tellSimpleString "subject") subjects
   tellSimpleMbString "type" mType
   mapM_ (tellSimpleString "format") format
   tellSimpleMbString "source" source
   mapM_ (tellSimpleString "relation") relation
   mapM_ (tellSimpleString "coverage") coverage
   mapM_ (tellSimpleString "rights") rights


formatMetadata :: Metadata -> String
formatMetadata meta = toList . execWriter
   $ tellMetadata meta
