module Lustre.Compiler where


import Language.Lustre.Parser               ( parseProgramFromFileLatin1 )
import Language.Lustre.Transform.OrderDecls (quickOrderTopDecl)
import Language.Lustre.TypeCheck            ( quickCheckDecls )
import Language.Lustre.Transform.NoStatic   ( noConst )
import Language.Lustre.AST qualified as Lus
import Language.Lustre.Monad qualified as Lus

import Data.Set qualified as Set
import System.Directory qualified as Dir
import System.FilePath  ( takeBaseName, addExtension, (</>), (<.>) )
import System.Exit      ( die )
import Prettyprinter    ( Pretty(..) )

import Lustre.Compiler.Monad            ( PassM, runPassM
                                        , Config(..), mkConfig, dbgPrint )
import Lustre.Compiler.Options          ( Options(..), parseOptions, usageString )
import Lustre.Compiler.Passes.Normalize ( normalizeM )
import Lustre.Compiler.Passes.ToStc     ( toStcM     )
import Lustre.Compiler.Passes.Schedule  ( scheduleM  )
import Lustre.Compiler.Passes.ToObc     ( toObcM     )
import Lustre.Compiler.Passes.Codegen   ( codegenM   )

--------------------------------------------------------------------------------

compileCmd :: [String] -> IO ()
compileCmd argv =
  case parseOptions argv of
    Left err -> error err
    Right (opts, _rest) ->
      case optProgFile opts of
        Nothing -> die usageString
        Just fp -> do config <- mkConfig opts
                      compile config fp

compile :: Config -> FilePath -> IO ()
compile config fp =
  do fpAbs <- Dir.makeAbsolute fp
     prg <- parseProgramFromFileLatin1 fp
     decls <- lusPasses config prg
     mbCompiled <- runPassM config (passes config decls)
     case mbCompiled of
       Left err ->
         error (show err)
       Right cCode ->
         do let baseName  = takeBaseName fpAbs
                cDir      = addExtension fpAbs "compiled"
                cFile     = cDir </> baseName <.> ".c"
            Dir.createDirectoryIfMissing True cDir
            wipeDirectory cDir
            writeFile cFile cCode
            let msg = "Writing C code to " ++ cFile ++ "."
            putStrLn msg

passes :: Config -> [Lus.TopDecl] -> PassM String
passes _config p0 =
  do p1 <- pass Identity        (pure . id)   p0
     p2 <- pass Normalization   normalizeM    p1
     p3 <- pass ToStc           toStcM        p2
     p4 <- pass Scheduling      scheduleM     p3
     p5 <- pass ToObc           toObcM        p4
     p6 <- pass Codegen         codegenM      p5
     pure (show p6)

pass :: (Pretty b) => Pass -> (a -> PassM b) -> a -> PassM b
pass name f x =
  do dbg "--------------------------------------------------------------------------------"
     dbg ("-- Running pass " ++ show name)
     dbg "--------------------------------------------------------------------------------"
     y <- f x
     dbg (show $ pretty y)
     pure y
  where
    dbg = dbgPrint passChatterLvl

-- | If the verbosity level is greater than or equal to this
--   the entire program will be printed out after each pass.
passChatterLvl :: Word
passChatterLvl = 3

data Pass
  = Identity
  | Normalization
  | ToStc
  | Scheduling
  | ToObc
  | Codegen
  deriving (Eq, Show)

lusPasses :: Config -> Lus.Program -> IO [Lus.TopDecl]
lusPasses config prg =
  Lus.runLustre lusConf (astPasses prg)
  where
  lusConf = Lus.LustreConf Nothing (cfgLogHandle config) Set.empty

  astPasses (Lus.ProgramPacks{})  =
    pure []
  astPasses (Lus.ProgramDecls ds) =
    do ds1 <- quickOrderTopDecl ds
       ds2 <- quickCheckDecls ds1
       (_, ds3) <- noConst ds2
       pure ds3

wipeDirectory :: FilePath -> IO ()
wipeDirectory fp =
  do entries <- Dir.listDirectory fp
     mapM_ Dir.removePathForcibly entries
