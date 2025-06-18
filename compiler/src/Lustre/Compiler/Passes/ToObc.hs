module Lustre.Compiler.Passes.ToObc
  ( toObcM, toObc ) where

import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.IR.Obc qualified as Obc
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Base
import Data.Text ( pack )

--------------------------------------------------------------------------------

toObcM :: Stc.Program -> PassM Obc.Program
toObcM = pure . toObc

toObc :: Stc.Program -> Obc.Program
toObc = fmap systemToClass

systemToClass :: Stc.SystemDecl -> Obc.ClassDecl
systemToClass (Stc.SystemDecl name binders tcs inits insts) =
  Obc.ClassDecl
    { Obc.clsName      = name
    , Obc.clsMemories  = mems
    , Obc.clsInstances = insts
    , Obc.clsMethods   = [ step, reset ]
    }
  where
    typeOfLit c = case c of
       Int{}  -> IntType
       Real{} -> RealType
       Bool{} -> BoolType

    mems    = map (\(lhs,c) ->
                     case lhs of
                       LVar x -> Binder x (typeOfLit c)
                       LSelect{} -> _todo) inits
    memVars = map binderDefines mems

    step = Obc.Method
      { Obc.mName    = fnStep
      , Obc.mBinders = fmap cType binders
      , Obc.mBody    = Obc.seqStmts $ map (tcToStmt memVars) tcs
      }

    reset = Obc.Method
      { Obc.mName    = fnReset
      , Obc.mBinders = mempty
      , Obc.mBody    = Obc.seqStmts $
                         map (\(x,c) -> Obc.LetState (fmap toAtom x) (Obc.Atom (Obc.Lit c))) inits ++
                         map (\(cls,i) -> Obc.LetCall [] (cls,i) fnReset []) insts
      }

tcToStmt :: [CompName] -> Stc.Tc -> Obc.Stmt
tcToStmt memVars tc = case tc of
  Stc.Define x clk cexpr ->
    ctrl memVars clk (cExprToStmt memVars x cexpr)
  Stc.Next x clk expr ->
    ctrl memVars clk (Obc.LetState (fmap toAtom x) (classifyVarsInExpr memVars expr))
  Stc.Call binds clk name args (ann,_) ->
    ctrl memVars clk (Obc.LetCall (map (fmap toAtom) binds) (name, ann) fnStep (map (Obc.Atom . classifyVarsInAtom memVars) args))

ctrl :: [CompName] -> Stc.Clock -> Obc.Stmt -> Obc.Stmt
ctrl memVars = go
  where
    go clk stmt = case clk of
      BaseClock  -> stmt
      WhenTrue x -> Obc.If (Obc.Atom (classifyVarsInAtom memVars x)) stmt Obc.Skip

cExprToStmt :: [CompName] -> Stc.LHS Stc.Atom -> Stc.CExpr -> Obc.Stmt
cExprToStmt memVars x cexpr = case cexpr of
  Stc.Expr e -> Obc.Let (fmap toAtom x) (goExpr e)
  _ -> _todo
  where
    -- go = cExprToStmt memVars x
    goExpr = classifyVarsInExpr memVars
    -- goAtom = classifyVarsInAtom memVars

classifyVarsInExpr :: [CompName] -> Stc.Expr -> Obc.Expr
classifyVarsInExpr memVars expr = case expr of
  Stc.Atom atom      -> Obc.Atom (goAtom atom)
  Stc.CallPrim pr ls -> Obc.CallPrim pr (map goAtom ls)
  _ -> error $ show expr
  where
    -- goExpr = classifyVarsInExpr memVars
    goAtom = classifyVarsInAtom memVars

classifyVarsInAtom :: [CompName] -> Stc.Atom -> Obc.Atom
classifyVarsInAtom memVars atom = case atom of
  Stc.Lit c _ -> Obc.Lit c
  Stc.Var x   -> if x `elem` memVars
                 then Obc.SVar x
                 else Obc.Var x

fnReset, fnStep :: CompName
fnReset = mkCompName' (pack "reset") Nothing ANode
fnStep = mkCompName' (pack "step") Nothing ANode

toAtom :: Stc.Atom -> Obc.Atom
toAtom a = case a of
  Stc.Lit c _ -> Obc.Lit c
  Stc.Var x   -> Obc.Var x
