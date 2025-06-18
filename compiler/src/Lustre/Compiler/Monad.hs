{-# OPTIONS_GHC -Wno-orphans #-}

module Lustre.Compiler.Monad
  ( module Lustre.Compiler.Monad
  , MonadGen(..) )
  where

import Control.Monad.Gen ( GenT, runGenT )
import Control.Monad.Gen.Class ( MonadGen(..) )
import Control.Monad.Except ( MonadError, ExceptT, runExceptT, throwError )
import Control.Monad.Reader ( ReaderT, runReaderT, ask )
import System.IO ( Handle, stdout, hPutStrLn, hFlush
                 , IOMode(..), openFile
                 )
import Lustre.Compiler.Options ( Options(..) )
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import System.Directory qualified as Dir

--------------------------------------------------------------------------------

type Unique = Integer

-- | The monad used by all compiler passes.
type PassM = GenT Unique (ReaderT Config (ExceptT Error IO))

instance Monad m => MonadFail (GenT e m) where
  fail = error

data Config = Config
  { cfgVerbosity :: Word
  , cfgLogHandle :: Handle
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgVerbosity = 0
  , cfgLogHandle = stdout
  }

mkConfig :: Options -> IO Config
mkConfig (Options verbosity _ mbLogfile) =
  case mbLogfile of
    Nothing -> pure cfg
    Just fp -> do fpAbs <- Dir.makeAbsolute fp
                  hdl <- openFile fpAbs WriteMode
                  putStrLn $ "Writing logs to " ++ fpAbs ++ "."
                  pure $ cfg { cfgLogHandle = hdl }
  where
    cfg = defaultConfig { cfgVerbosity = verbosity }

data Error
  = TCError String
  | AssertionError String
  | Other String
  deriving (Eq, Show)

newUniq :: MonadGen Unique m => m Unique
newUniq = gen

runPassM :: Config -> PassM a -> IO (Either Error a)
runPassM cfg = runExceptT . (\m -> runReaderT m cfg) . runGenT

reportError :: MonadError Error m => Error -> m a
reportError = throwError

-- | Like 'assert', but doesn't raise an exception.
assertOrError :: MonadError Error m => String -> Bool -> a -> m a
assertOrError msg b ret =
  if b then pure ret else reportError (AssertionError msg)

dbgPrint :: Word -> String -> PassM ()
dbgPrint lvl msg =
  do config <- ask
     when (cfgVerbosity config >= lvl) $
       do let hdl = cfgLogHandle config
          liftIO $ hPutStrLn hdl msg
          liftIO $ hFlush hdl
