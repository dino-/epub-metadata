#! /usr/bin/runhaskell -isrc

-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Environment ( getArgs )

import Codec.Epub.Opf.Parse


main :: IO ()
main = (fmap head) getArgs >>= readFile >>= parseXmlToOpf >>= print
