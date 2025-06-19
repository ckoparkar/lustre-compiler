module Lustre.Compiler.Passes.ToObc
  ( toObcM, toObc ) where

import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.IR.Obc qualified as Obc
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Base
import Data.Text ( pack )
import Data.Map qualified as Map

--------------------------------------------------------------------------------

toObcM :: Stc.Program -> PassM Obc.Program
toObcM = pure . toObc

toObc :: Stc.Program -> Obc.Program
toObc = fmap (classifyVars . explicitCopies . systemToClass)

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

    outVars = map binderDefines (nodeOutputs binders)

    step = Obc.Method
      { Obc.mName    = fnStep
      , Obc.mBinders = fmap cType binders
      , Obc.mBody    = Obc.seqStmts $ map tcToStmt tcs
      }

    reset = Obc.Method
      { Obc.mName    = fnReset
      , Obc.mBinders = mempty
      , Obc.mBody    = Obc.seqStmts $
                         map (\(x,c) -> Obc.LetState (toObcLHS x) (Obc.Atom (Obc.Lit c))) inits ++
                         map (\(cls,i) -> Obc.LetCall [] (cls,i) fnReset []) insts
      }

fnReset, fnStep :: CompName
fnReset = mkCompName' (pack "reset") Nothing ANode
fnStep = mkCompName' (pack "step") Nothing ANode

tcToStmt :: Stc.Tc -> Obc.Stmt
tcToStmt tc = case tc of
  Stc.Define x clk cexpr ->
    ctrl clk (cExprToStmt x cexpr)
  Stc.Next x clk expr ->
    ctrl clk (Obc.LetState (toObcLHS x) (toObcExpr expr))
  Stc.Call binds clk name args (ann,_) ->
    ctrl clk (Obc.LetCall (map toObcLHS binds)
                          (name, ann)
                          fnStep
                          (map (Obc.Atom . toObcAtom) args))

ctrl :: Stc.Clock -> Obc.Stmt -> Obc.Stmt
ctrl clk stmt = case clk of
  BaseClock  -> stmt
  WhenTrue x -> Obc.If (Obc.Atom (toObcAtom x)) stmt Obc.Skip

cExprToStmt :: Stc.LHS Stc.Atom -> Stc.CExpr -> Obc.Stmt
cExprToStmt x cexpr = case cexpr of
  Stc.Expr e ->
    case e of
      Stc.UpdateStruct tyName from updates ->
        Obc.Do
          (Obc.LetCopyStruct (toObcLHS x) (toVar from) tyName)
          (Obc.UpdateFields (toObcLHS x) (map (fmap toObcAtom) updates))
      _oth ->
        Obc.Let (toObcLHS x) (goExpr e)
  _ -> _todo
  where
    goExpr = toObcExpr

    toVar from = case from of
      Stc.Var x -> Obc.Oth x
      Stc.Lit{} -> bad ("Not a variable " ++ show from)

toObcExpr :: Stc.Expr -> Obc.Expr
toObcExpr expr = case expr of
  Stc.Atom atom      -> Obc.Atom (goAtom atom)
  Stc.CallPrim pr ls -> Obc.CallPrim pr (map goAtom ls)
  _ -> _todo
  where
    goAtom = toObcAtom

toObcAtom :: Stc.Atom -> Obc.Atom
toObcAtom atom = case atom of
  Stc.Lit c _ -> Obc.Lit c
  Stc.Var x   -> Obc.Var (Obc.Oth x)

toObcLHS :: Stc.LHS Atom -> Obc.LHS Obc.Atom
toObcLHS lhs = case lhs of
  Stc.LVar x      -> Obc.LVar (Obc.Oth x)
  Stc.LSelect l s -> Obc.LSelect (toObcLHS l) (fmap toObcAtom s)

--------------------------------------------------------------------------------

classifyVars :: Obc.ClassDecl -> Obc.ClassDecl
classifyVars cls = cls { Obc.clsMethods = map goMthd (Obc.clsMethods cls) }
  where
    goMthd (Obc.Method mname binders stmt0) =
      Obc.Method mname binders (go stmt0)
      where
        memVars = map binderDefines (Obc.clsMemories cls)
        outVars = map binderDefines (Obc.nodeOutputs binders)

        go stmt = case stmt of
          Obc.Skip                          -> Obc.Skip
          Obc.Do s1 s2                      -> Obc.Do (go s1) (go s2)
          Obc.If cnd thn els                -> Obc.If (goExpr cnd) (go thn) (go els)
          Obc.UpdateFields lhs updates      -> Obc.UpdateFields (goLHS lhs) (map goField updates)
          Obc.Let lhs expr                  -> Obc.Let (goLHS lhs) (goExpr expr)
          Obc.LetState lhs expr             -> Obc.LetState (goLHS lhs) (goExpr expr)
          Obc.LetCopyStruct lhs from tyname -> Obc.LetCopyStruct (goAddrOf (goLHS lhs)) (Obc.Addr (goVar from)) tyname
          Obc.LetCall binds cl rator rands  -> Obc.LetCall (map goLHS binds) cl rator (map goExpr rands)

        goExpr expr = case expr of
          Obc.Atom atom      -> Obc.Atom (goAtom atom)
          Obc.CallPrim pr ls -> Obc.CallPrim pr (map goAtom ls)

        goAtom atom = case atom of
          Obc.Lit c -> Obc.Lit c
          Obc.Var x -> Obc.Var (goVar x)

        goVar x = case x of
          Obc.Oth y
            | y `elem` memVars -> Obc.State y
            | y `elem` outVars -> Obc.Out y
            | otherwise        -> Obc.Oth y

          _ -> bad (show x)

        goField (Obc.Field fname fval) = Obc.Field fname (goAtom fval)

        goLHS lhs = case lhs of
          Obc.LVar v      -> Obc.LVar (goVar v)
          Obc.LSelect l s -> Obc.LSelect (goLHS l) (fmap goAtom s)

        goAddrOf lhs = case lhs of
          Obc.LVar v -> Obc.LVar (Obc.Addr v)
          Obc.LSelect l s -> Obc.LSelect (goAddrOf l) (fmap goAtom s)


explicitCopies :: Obc.ClassDecl -> Obc.ClassDecl
explicitCopies cls = cls { Obc.clsMethods = map goMthd (Obc.clsMethods cls) }
  where
    goMthd (Obc.Method mname binders stmt) =
      let env = Map.fromList $ map (\(Binder x ty) -> (x,ty)) (allBinders binders)
      in Obc.Method mname binders (go env stmt)

    go env stmt = case stmt of
      Obc.Do s1 s2     -> Obc.Do (go env s1) (go env s2)
      Obc.Let lhs expr -> case expr of
                            Obc.Atom (Obc.Var v) -> case env Map.! (Obc.varCompName v) of
                              NamedType tyname -> Obc.LetCopyStruct lhs v tyname
                              _ -> stmt
                            _ -> stmt
      _ -> stmt

bad :: String -> a
bad msg = error ("Unexpected " ++ msg)
