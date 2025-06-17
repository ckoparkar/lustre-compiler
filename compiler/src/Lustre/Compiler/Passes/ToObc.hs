module Lustre.Compiler.Passes.ToObc
  ( toObcM, toObc ) where

import Language.Lustre.Name ( identText )
import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.IR.Obc qualified as Obc
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Lustre ( nameToIdent )
import Lustre.Compiler.IR.Base
import Data.Text ( Text, pack )

--------------------------------------------------------------------------------

toObcM :: Stc.Program -> PassM Obc.Program
toObcM = pure . toObc

toObc :: Stc.Program -> Obc.Program
toObc (Stc.Program decls) = Obc.Program (map go decls)
  where
    go d = case d of
      Stc.DeclareType t  -> Obc.DeclareType t
      Stc.DeclareConst c -> Obc.DeclareConst c
      Stc.DeclareNode nd -> Obc.DeclareNode (systemToClass nd)


systemToClass :: Stc.SystemDecl -> Obc.ClassDecl
systemToClass (Stc.SystemDecl name binders tcs inits insts) =
  Obc.ClassDecl
    { Obc.clsName      = identText name
    , Obc.clsMemories  = mems
    , Obc.clsInstances = map (\(n,i) -> (identText (nameToIdent n), identText i)) insts
    , Obc.clsMethods   = [ step, reset ]
    }
  where
    typeOfLit c = case c of
       Int{}  -> IntType
       Real{} -> RealType
       Bool{} -> BoolType

    mems    = map (\(lhs,c) ->
                     case lhs of
                       LVar x -> Binder x (CType (typeOfLit c) (error "REMOVEME"))
                       LSelect{} -> error $ "systemToClass: TODO " ++ show lhs) inits
    memVars = map binderDefines mems

    step = Obc.Method
      { Obc.mName    = fnStep
      , Obc.mBinders = binders
      , Obc.mBody    = Obc.seqStmts $ map (tcToStmt memVars) tcs
      }

    reset = Obc.Method
      { Obc.mName    = fnReset
      , Obc.mBinders = NodeBinders [] [] []
      , Obc.mBody    = Obc.seqStmts $
                         map (\(x,c) -> Obc.LetState x (Obc.Lit c) ) inits ++
                         map (\(cls,i) -> Obc.LetCall [] (cls,i) fnReset []) insts
      }

tcToStmt :: [Ident] -> Stc.Tc -> Obc.Stmt
tcToStmt memVars tc = case tc of
  Stc.Define x clk cexpr ->
    ctrl memVars clk (cExprToStmt memVars x cexpr)
  Stc.Next x clk expr ->
    ctrl memVars clk (Obc.LetState x (classifyVarsInExpr memVars expr))
  Stc.Call binds clk name args (ann,_) ->
    ctrl memVars clk (Obc.LetCall binds (name, ann) fnStep (map (classifyVarsInAtom memVars) args))

ctrl :: [Ident] -> Stc.Clock -> Obc.Stmt -> Obc.Stmt
ctrl memVars = go
  where
    go clk stmt = case clk of
      BaseClock  -> stmt
      WhenTrue x -> Obc.If (classifyVarsInAtom memVars x) stmt Obc.Skip

cExprToStmt :: [Ident] -> Stc.LHS Stc.Atom -> Stc.CExpr -> Obc.Stmt
cExprToStmt memVars x cexpr = case cexpr of
  Stc.Expr e -> Obc.Let x (goExpr e)
  _ -> error $ "cExprToStmt: TODO " ++ show cexpr
  where
    go = cExprToStmt memVars x
    goExpr = classifyVarsInExpr memVars
    goAtom = classifyVarsInAtom memVars

classifyVarsInExpr :: [Ident] -> Stc.Expr -> Obc.Expr
classifyVarsInExpr memVars expr = case expr of
  Stc.Atom atom       -> goAtom atom
  Stc.CallPrim pr ls  -> Obc.CallPrim pr (map goAtom ls)
  _ -> error $ "classifyVarsInExpr: TODO " ++ show expr
  where
    goExpr = classifyVarsInExpr memVars
    goAtom = classifyVarsInAtom memVars

classifyVarsInAtom :: [Ident] -> Stc.Atom -> Obc.Expr
classifyVarsInAtom memVars atom = case atom of
  Stc.Lit c _         -> Obc.Lit c
  Stc.Var x           -> if nameToIdent x `elem` memVars
                         then Obc.SVar x
                         else Obc.Var x

fnReset, fnStep :: Text
fnReset = pack "reset"
fnStep  = pack "step"
