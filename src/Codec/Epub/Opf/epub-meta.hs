-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Applicative ( (<$>) )
import Control.Monad.Error
import System.Environment ( getArgs )

import Codec.Epub.Opf.Metadata.Parse


main :: IO ()
main = do
   zipPath <- head <$> getArgs

   result <- runErrorT $ parseEpubMeta zipPath
   print result

   return ()
