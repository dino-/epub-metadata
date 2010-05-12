-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad.Error
import System.Environment ( getArgs )
import System.Exit

import Codec.Epub.Opf.Metadata.Format
import Codec.Epub.Opf.Metadata.Parse


main :: IO ()
main = do
   as <- getArgs

   when (length as /= 1) $ do
      putStrLn "usage: epub-meta EPUBFILE"
      exitWith $ ExitFailure 1

   let zipPath = head as
   result <- runErrorT $ parseEpubOpf zipPath

   putStr $ either (++ "\n") opfToString result
