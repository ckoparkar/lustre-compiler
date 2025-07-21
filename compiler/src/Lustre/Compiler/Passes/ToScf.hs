module Lustre.Compiler.Passes.ToScf
  ( toScfM ) where

import Control.Monad.Reader qualified as Rd
import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.IR.Scf qualified as Scf
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Base
import Lustre.Utils ( todo )
import Data.Text ( pack )

--------------------------------------------------------------------------------

toScfM :: Stc.Program -> PassM Scf.Program
toScfM = traverse toScfSystem

data ClassState = ClassState
  { stClsMembers :: [CompName]
  }
  deriving Show

instance Semigroup ClassState where
  (ClassState s1) <> (ClassState s2) = ClassState (s1 <> s2)

instance Monoid ClassState where
  mempty = ClassState []

type M a = Rd.ReaderT ClassState PassM a

runM :: M a -> ClassState -> PassM a
runM = Rd.runReaderT

--------------------------------------------------------------------------------

toScfSystem :: Stc.SystemDecl -> PassM Scf.ClassDecl
toScfSystem (Stc.SystemDecl name binders tcs inits insts) =
  do let st = ClassState (map fst3 inits)
     step <- runM mstep st
     reset <- runM mreset st
     pure Scf.ClassDecl
            { Scf.clsName      = name
            , Scf.clsInits     = inits
            , Scf.clsInstances = insts
            , Scf.clsMethods   = [step, reset]
            }
  where
    fst3 (a,_,_) = a
    mstep =
      do let fnName = mkCompName' (pack "step") Nothing ANode
         tcsStmts <- mapM tcToStmt tcs
         let stmts = -- map (\(Binder x cty) -> Scf.Declare [LVar x] (toScfType cty))
                     --     (nodeOutputs binders ++ nodeLocals binders) ++
                     tcsStmts ++
                     [Scf.Return (map (Scf.Var . Scf.Reg . binderDefines) (nodeOutputs binders))]
         pure Scf.Method { Scf.mName = fnName
                         , Scf.mIns  = fmap (fmap toScfType) (nodeInputs binders)
                         , Scf.mOuts = map (toScfType . binderType) (nodeOutputs binders)
                         , Scf.mBody = stmts
                         }

    mreset =
      do let fnName = mkCompName' (pack "reset") Nothing ANode
         pure Scf.Method { Scf.mName = fnName
                         , Scf.mIns  = []
                         , Scf.mOuts = []
                         , Scf.mBody = map (\(x,c,ty) -> Scf.Assign [Scf.LVar (Scf.ClsMember x)] (Scf.Expr (Scf.Atom (Scf.Lit c ty)))) inits ++
                                       map (\(Stc.NodeInstInfo cls i) -> Scf.Call cls fnName [] i []) insts

                         }

--------------------------------------------------------------------------------

ctrl :: Stc.Clock -> Scf.Stmt -> M Scf.Stmt
ctrl clk cexpr = case clk of
  Stc.BaseClock  -> pure cexpr
  Stc.WhenEq a b -> do a1 <- toScfAtom a
                       b1 <- toScfAtom b
                       pure $ Scf.If (Scf.CallPrim (Scf.Op2 Scf.Eq) [Scf.Atom a1, Scf.Atom b1] BoolType)
                                     cexpr Nothing

tcToStmt :: Stc.Tc -> M Scf.Stmt
tcToStmt tc = case tc of
  Stc.Define x clk cexpr ->
    do stmt <- toScfStmt x cexpr
       ctrl clk stmt
  Stc.Next x clk expr ->
    do stmt <- toScfStmt x (Stc.Expr expr)
       ctrl clk stmt
  Stc.Call binds clk name args inst ret ->
    do args1 <- mapM toScfExpr args
       let fnName = mkCompName' (pack "step") Nothing ANode
       let stmt = Scf.Call name fnName args1 inst (map toScfType ret)
       ctrlStmt <- ctrl clk stmt
       binds1 <- mapM toScfLHS binds
       pure (Scf.Define binds1 ctrlStmt)

toScfStmt :: LHS Stc.Expr -> Stc.CExpr -> M Scf.Stmt
toScfStmt x cexpr = case cexpr of
  Stc.Expr e ->
    do e1 <- toScfExpr e
       let stmt = Scf.Expr e1
       x1 <- toScfLHS x
       if isLHSField x1
         then pure $ Scf.Assign [x1] stmt
         else pure $ Scf.Define [x1] stmt
  Stc.If cnd thn els ->
    do xthn <- mkCompName (pack "_x") Nothing AVal
       xels <- mkCompName (pack "_x") Nothing AVal
       cnd1 <- toScfExpr cnd
       thn1 <- toScfExpr thn
       els1 <- toScfExpr els
       let stmt = Scf.If cnd1
                         (Scf.Do (Scf.Define [Scf.LVar (Scf.Reg xthn)] (Scf.Expr thn1))
                                 (Scf.Yield [Scf.Var (Scf.Reg xthn)]))
                         (Just (Scf.Do (Scf.Define [Scf.LVar (Scf.Reg xels)] (Scf.Expr els1))
                                       (Scf.Yield [Scf.Var (Scf.Reg xels)])))
       x1 <- toScfLHS x
       if isLHSField x1
         then pure $ Scf.Assign [x1] stmt
         else pure $ Scf.Define [x1] stmt
  Stc.Merge (cnd,_) ls ->
    do ys <- mapM (\_ -> mkCompName (pack "_x") Nothing AVal) ls
       ls1 <- mapM (\(y, (c,e)) ->
                      do e1 <- toScfExpr e
                         let rhs = Scf.Do (Scf.Define [Scf.LVar (Scf.Reg y)] (Scf.Expr e1))
                                          (Scf.Yield [Scf.Var (Scf.Reg y)])
                         pure (c, rhs))
                     (zip ys ls)
       let stmt = Scf.Switch (Scf.Atom (Scf.Var (Scf.Reg cnd))) ls1
       x1 <- toScfLHS x
       if isLHSField x1
         then pure $ Scf.Assign [x1] stmt
         else pure $ Scf.Define [x1] stmt
  where
    isLHSField lhs = case lhs of
      Scf.LVar (Scf.Reg{})       -> False
      Scf.LVar (Scf.ClsMember{}) -> True
      Scf.LSelect lhs1 _         -> isLHSField lhs1

toScfExpr :: Stc.Expr -> M Scf.Expr
toScfExpr expr = case expr of
  Stc.Atom atom         -> Scf.Atom <$> toScfAtom atom
  Stc.CallPrim pr ls ty -> do ls1 <- mapM toScfExpr ls
                              pure $ Scf.CallPrim pr ls1 (Stc.cType ty)
  Stc.Select x sel      -> do x1 <- toScfAtom x
                              sel1 <- traverse toScfExpr sel
                              pure $ Scf.Select x1 sel1
  Stc.When e _          -> toScfExpr e
  Stc.Struct tyName updates ->
    do updates1 <- mapM toScfField updates
       pure $ Scf.Struct tyName updates1
  Stc.UpdateStruct tyName from updates ->do
    do from1 <- toScfAtom from
       updates1 <- mapM toScfField updates
       pure $ Scf.UpdateStruct tyName from1 updates1
  _ -> todo expr

toScfField :: Field Stc.Expr -> M (Field Scf.Expr)
toScfField (Field name val) = do val1 <- toScfExpr val
                                 pure (Field name val1)

toScfAtom :: Stc.Atom -> M Scf.Atom
toScfAtom atom = case atom of
  Stc.Lit c ty -> pure (Scf.Lit c (toScfType ty))
  Stc.Var x   -> do st <- Rd.ask
                    if x `elem` (stClsMembers st)
                      then pure $ Scf.Var (Scf.ClsMember x)
                      else pure $ Scf.Var (Scf.Reg x)

toScfLHS :: Stc.LHS Stc.Expr -> M (Scf.LHS Scf.Expr)
toScfLHS lhs = case lhs of
  Stc.LVar x      -> do st <- Rd.ask
                        if x `elem` (stClsMembers st)
                          then pure $ Scf.LVar (Scf.ClsMember x)
                          else pure $ Scf.LVar (Scf.Reg x)
  Stc.LSelect l s -> do s1 <- traverse toScfExpr s
                        l1 <- toScfLHS l
                        pure $ Scf.LSelect l1 s1

toScfType :: Stc.CType -> Scf.Type
toScfType = Stc.cType
