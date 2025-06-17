module Lustre.Compiler where


import Language.Lustre.Parser               ( parseProgramFromFileLatin1 )
import Language.Lustre.Pretty               ( showPP )
import Language.Lustre.Transform.OrderDecls (quickOrderTopDecl)
import Language.Lustre.TypeCheck            ( quickCheckDecls )
import Language.Lustre.Transform.NoStatic   ( noConst )
import Language.Lustre.AST qualified as Lus
import Language.Lustre.Monad qualified as Lus
import System.Exit                      ( die )
import Data.Set qualified as Set

import Lustre.Compiler.Monad            ( PassM, runPassM
                                        , Config(..), mkConfig, dbgPrint )
import Lustre.Compiler.Options          ( Options(..), parseOptions, usageString )
import Lustre.Compiler.Passes.Normalize ( normalizeM )
import Prettyprinter ( Pretty(..) )

-- TEMP
import Lustre.Compiler.IR.NLustre.Syntax qualified as NL

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
  do prg <- parseProgramFromFileLatin1 fp
     decls <- lusPasses config prg
     mbCompiled <- runPassM config (passes config decls)
     case mbCompiled of
       Left err ->
         error (show err)
       Right compiled ->
         do print (pretty compiled)

passes :: Config -> [Lus.TopDecl] -> PassM NL.Program
passes config decls0 =
  do decls1 <- normalizeM decls0
     pure decls1

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
