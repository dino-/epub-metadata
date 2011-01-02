-- Copyright: 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Codec.Epub.Opf.Cli.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import System.Console.GetOpt


data Options = Options
   { optHelp :: Bool
   , optVerbose :: Bool
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optVerbose = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True } )) 
      "Display all OPF package info, including manifest, spine and guide"
   ]


parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: epub-meta [OPTIONS] EPUBFILE"
         , "Examine ePub OPF package data"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Version 2.0.0  Dino Morelli <dino@ui3.info>"
         ]
