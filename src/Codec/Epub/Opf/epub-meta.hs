-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Codec.Archive.Zip
import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory ( doesFileExist )
import System.Environment ( getArgs )

import Codec.Epub.Opf.Metadata


main :: IO ()
main = do
   zipPath <- head <$> getArgs

   exists <- doesFileExist zipPath
   archive <- if exists
      then toArchive <$> B.readFile zipPath
      else return emptyArchive

   B.putStrLn $ opfPath archive

   --mapM_ putStrLn $ filesInArchive archive

   print $ extractEpubMeta archive

   return ()
