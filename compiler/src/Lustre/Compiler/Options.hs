module Lustre.Compiler.Options
  ( Options(..), parseOptions, usageString ) where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

data Options = Options
  { optVerbosity :: Word
  , optProgFile  :: Maybe FilePath
  , optLogFile   :: Maybe FilePath
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optVerbosity = 0
  , optProgFile  = Nothing
  , optLogFile   = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbosity"]
      (OptArg ((\n opts -> opts { optVerbosity = n }) . read . fromMaybe "verbosity")
              "N")
      "verbosity N"
  , Option ['p'] ["prog"]
      (OptArg ((\ f opts -> opts { optProgFile = Just f }) . fromMaybe "program")
              "FILE")
      "program FILE"
  , Option ['l'] ["log"]
      (OptArg ((\ f opts -> opts { optLogFile = Just f }) . fromMaybe "log")
              "FILE")
      "log FILE"
  ]

parseOptions :: [String] -> Either String (Options, [String])
parseOptions argv =
   case getOpt Permute options argv of
         (o,n,[]  ) -> Right (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> Left  (concat errs ++ usageInfo header options)
     where header = "Usage: ic [OPTION...] files..."

usageString :: String
usageString = usageInfo "lustre-compiler: " options
