module Lustre.Compiler.Passes.Normalize
  (normalizeM) where


import Language.Lustre.AST qualified as Lus
import Language.Lustre.Name qualified as Lus
import Control.Monad.State qualified as St
import Control.Monad ( foldM )

import Lustre.Compiler.IR.Lustre
import Lustre.Compiler.IR.NLustre qualified as NL
import Lustre.Compiler.Monad

--------------------------------------------------------------------------------

normalizeM :: [Lus.TopDecl] -> PassM NL.Program
normalizeM p0 = toAnf p0 >>= normFby >>= toNL

normFby :: [Lus.TopDecl] -> PassM [Lus.TopDecl]
normFby = traversePrg normFbyExpr

toAnf :: [Lus.TopDecl] -> PassM [Lus.TopDecl]
toAnf = traversePrg toAnfExpr

toNL :: [Lus.TopDecl] -> PassM NL.Program
toNL decls = NL.Program <$> mapM toDecl decls
  where
    toDecl decl = case decl of
      Lus.DeclareType tydecl -> pure (NL.DeclareType tydecl)
      Lus.DeclareConst c     -> pure (NL.DeclareConst c)
      Lus.DeclareNode nd     -> pure (NL.DeclareNode $ toNode nd)
      Lus.DeclareNodeInst{}  -> reportError (Other "normalize: unexpected DeclareNodeInst")
      Lus.DeclareContract{}  -> reportError (Other "normalize: unexpected DeclareContract")

    toNode nd =
      let eqns = case Lus.nodeDef nd of
                   Nothing -> []
                   Just (Lus.NodeBody _ eqs) -> concatMap toEqn eqs
          (ins, outs, locals) = nodeBinders nd
      in NL.NodeDecl (Lus.nodeName nd)
                     (NL.NodeBinders (map toBinder ins)
                                     (map toBinder outs)
                                     (map toBinder locals))
                     eqns

    toBinder (Lus.Binder x ty) =
      NL.Binder x (toCType ty)

    toCType (Lus.CType ty clk) =
      NL.CType ty (toClock clk)

    toClock clk = case clk of
      Lus.BaseClock    -> NL.BaseClock
      Lus.ClockVar{}   -> error $ "toClock: Unexpected " ++ show clk
      Lus.KnownClock e -> NL.WhenTrue (toClockExpr e)

    toClockExpr (Lus.WhenClock _ e nm) =
      let a1 = toLit e
          a2 = NL.Var (NL.Unqual nm)
      in case a1 of
        NL.Bool True -> a2
        _            -> error $ "toClockExpr: TODO " ++ show a1

    toLit expr = case expr of
      Lus.ERange _ e -> toLit e
      Lus.Const e _  -> toLit e
      Lus.Lit c      -> c
      Lus.Var{}      -> error $ "toLit: TODO " ++ show expr
      _              -> error $ "toLit: Unexpected " ++ show expr

    toEqn eqn = case eqn of
      Lus.Define lhs rhs -> [NL.Define (map toLHS lhs) (toRHS rhs)]
      _                  -> []

    toLHS lhs = case lhs of
      Lus.LVar x        -> NL.LVar x
      Lus.LSelect x sel -> NL.LSelect (toLHS x) (fmap toAtom sel)

    toRHS expr = case expr of
      Lus.ERange _ e -> toRHS e
      Lus.Call (Lus.NodeInst fn []) args _clk ctys ->
        case fn of
          Lus.CallUser nm     -> NL.Call nm (map toAtom args) (fmap (fmap toCType) ctys)
          Lus.CallPrim _ prim -> case (prim, args) of
            (Lus.Op2 Lus.Fby, [Lus.Const (Lus.Lit c) _, next]) -> NL.Fby2 c (toExpr next)
            _ -> NL.CExpr (toCExpr expr)
      _ -> NL.CExpr (toCExpr expr)

    toCExpr expr = case expr of
      Lus.Merge{} -> error "toCExpr: TODO Merge"
      Lus.Call (Lus.NodeInst fn []) args _clk _ctys ->
        case fn of
          Lus.CallUser{} -> error $ "toCExpr: " ++ show expr
          Lus.CallPrim _ prim -> case (prim, args) of
            (Lus.ITE, [cnd, thn, els]) -> NL.If (toAtom cnd) (toExpr thn) (toExpr els)
            _ -> NL.Expr (toExpr expr)
      _ -> NL.Expr (toExpr expr)

    toExpr expr = case expr of
      Lus.ERange _ e -> toExpr e
      Lus.Var{}      -> NL.Atom (toAtom expr)
      Lus.Lit{}      -> NL.Atom (toAtom expr)
      Lus.Const{}    -> NL.Atom (toAtom expr)
      Lus.When{}     -> error "toExpr: TODO When"
      Lus.Tuple ls   -> NL.Tuple (map toAtom ls)
      Lus.Array ls   -> NL.Array (map toAtom ls)
      Lus.Select e sel         -> NL.Select (toAtom e) (fmap toAtom sel)
      Lus.Struct nm ls         -> NL.Struct nm (map (fmap toAtom) ls)
      Lus.UpdateStruct nm e ls -> NL.UpdateStruct nm (toAtom e) (map (fmap toAtom) ls)
      Lus.WithThenElse{}       -> error "toExpr: TODO WithThenElse"
      Lus.Merge{}              -> error $ "toExpr: not an expression " ++ show expr
      Lus.Call (Lus.NodeInst fn []) args _clk _ctys ->
        let args1 = map toAtom args in
          case fn of
            Lus.CallUser{}      -> error $ "toExpr: " ++ show expr
            Lus.CallPrim _ prim -> NL.CallPrim prim args1
      Lus.Call (Lus.NodeInst _ (_x:_xs)) _args _clk __ctys ->
        error $ "toExpr: static arguments not empty, " ++ show expr

    toAtom expr = case expr of
      Lus.Var x                -> NL.Var x
      Lus.Const (Lus.Lit c) ty -> NL.Lit c (toCType ty)
      _ -> error $ "toAtom: " ++ show expr

--------------------------------------------------------------------------------

data NodeState = NodeState
  { nLocals :: [Lus.Binder] }
  deriving Show

instance Semigroup NodeState where
  (NodeState a) <> (NodeState z) = NodeState (a <> z)

instance Monoid NodeState where
  mempty = NodeState mempty

type M a = St.StateT NodeState PassM a

runM :: M a -> NodeState -> PassM (a, NodeState)
runM = St.runStateT

addLocal :: Lus.Ident -> Lus.CType -> M ()
addLocal x ty =
  St.modify (\ns -> ns { nLocals = (Lus.Binder x ty) : (nLocals ns) })

typeOfM :: TypeOf a => Lus.NodeDecl -> a -> M [Lus.CType]
typeOfM nd e =
  do st <- St.get
     let locals = map Lus.LocalVar (nLocals st)
         nd1    = bindLocals locals nd
     pure (typeOf nd1 e)

--------------------------------------------------------------------------------

normFbyExpr :: Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation])
normFbyExpr _nd expr
  | Lus.Call (Lus.NodeInst fn []) [e0, enext] clk (Just [ty]) <- expr
  , Lus.CallPrim range prim <- fn
  , Lus.Op2 Lus.Fby <- prim
  = case e0 of
      Lus.Const{} -> pure (expr, [])
      Lus.Var{}   ->
        do px    <- freshIdent
           xinit <- freshIdent
           res   <- freshIdent
           mapM_ (uncurry addLocal) [ (xinit, (Lus.CType Lus.BoolType Lus.BaseClock))
                                    , (px, ty)
                                    , (res, ty)]
           initConst <- case ty of
                          (Lus.CType Lus.IntType _)  -> pure $ Lus.Lit (Lus.Int 0)
                          (Lus.CType Lus.BoolType _) -> pure $ Lus.Lit (Lus.Bool True)
                          (Lus.CType Lus.RealType _) -> pure $ Lus.Lit (Lus.Real 0.0)
                          _ -> reportError (Other $ "normFbyExpr: unexpected initial type, " ++ show ty)
           let true  = Lus.Const (Lus.Lit (Lus.Bool True)) (Lus.CType Lus.BoolType Lus.BaseClock)
               false = Lus.Const (Lus.Lit (Lus.Bool False)) (Lus.CType Lus.BoolType Lus.BaseClock)
               eqn1  = Lus.Define [Lus.LVar xinit]
                                  (Lus.Call (Lus.NodeInst (Lus.CallPrim range (Lus.Op2 Lus.Fby)) [])
                                            [true, false]
                                            clk
                                            (Just [Lus.CType Lus.BoolType Lus.BaseClock]))
               eqn2  = Lus.Define [Lus.LVar px]
                                  (Lus.Call (Lus.NodeInst (Lus.CallPrim range (Lus.Op2 Lus.Fby)) [])
                                            [initConst, enext]
                                            clk
                                            (Just [ty]))
               eqn3  = Lus.Define [Lus.LVar res]
                                  (Lus.Call (Lus.NodeInst (Lus.CallPrim range Lus.ITE) [])
                                            [Lus.Var (Lus.Unqual xinit), e0, Lus.Var (Lus.Unqual px)]
                                            clk
                                            (Just [ty]))
           pure ( Lus.Var (Lus.Unqual res), [eqn1, eqn2, eqn3] )
      _ -> reportError (Other $ "normFbyExpr: not ANF, " ++ show expr)
  | otherwise
  = pure (expr, [])

toAnfExpr :: Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation])
toAnfExpr nd expr = case expr of
  Lus.Lit{}   -> reportError (Other $ "Unexpected: " ++ show expr)
  Lus.Var{}   -> pure (expr, [])
  Lus.Const{} -> pure (expr, [])
  Lus.ERange _ e -> go e
  expr1 `Lus.When` clk ->
    do (expr2, eqns) <- go expr1
       let rhs = Lus.When expr2 clk
       bind rhs eqns
  Lus.Tuple args ->
    do (args1, eqns) <- toAnfExprs args
       let rhs = Lus.Tuple args1
       bind rhs eqns
  Lus.Array args ->
    do (args1, eqns) <- toAnfExprs args
       let rhs = Lus.Array args1
       bind rhs eqns
  Lus.Select expr1 selector ->
    do (expr2, eqns1) <- go expr1
       (selector1, eqns2) <- toAnfSelector selector
       let rhs = Lus.Select expr2 selector1
       bind rhs (eqns1 ++ eqns2)
  Lus.Struct nm fields ->
    do (fields1, eqns) <- foldOnExprs toAnfField nd fields
       let rhs = Lus.Struct nm fields1
       bind rhs eqns
  Lus.UpdateStruct nm expr1 fields ->
    do (fields1, eqns1) <- foldOnExprs toAnfField nd fields
       (expr2, eqns2) <- go expr1
       let rhs = Lus.UpdateStruct nm expr2 fields1
       bind rhs (eqns1 ++ eqns2)
  Lus.Call fn args clk ctys ->
    do (args1, eqns) <- toAnfExprs args
       let rhs = Lus.Call fn args1 clk ctys
       bind rhs eqns
  Lus.Merge nm alts ->
    do (alts1, eqns) <- foldOnExprs toAnfAlt nd alts
       let rhs = Lus.Merge nm alts1
       bind rhs eqns
  Lus.WithThenElse{} -> reportError (Other $ "TODO: " ++ show expr)
  where
    go = toAnfExpr nd
    toAnfExprs = foldOnExprs toAnfExpr nd

    bind rhs eqns =
      do ident <- freshIdent
         let nm  = Lus.Unqual ident
             lhs = Lus.LVar ident
             eqn =  Lus.Define [lhs] rhs
         [ty]  <- typeOfM nd rhs
         addLocal ident ty
         pure (Lus.Var nm, eqn:eqns)

    toAnfAlt _nd (Lus.MergeCase pat rhs) =
      do (pat1, eqns1) <- go pat
         (rhs1, eqns2) <- go rhs
         pure (Lus.MergeCase pat1 rhs1, eqns1 ++ eqns2)

    toAnfField _nd (Lus.Field fnm fval) =
      do (fval1, eqns) <- go fval
         pure (Lus.Field fnm fval1, eqns)

    toAnfSelector selector = case selector of
      Lus.SelectField{} -> pure (selector, [])
      Lus.SelectElement expr1 ->
        do (expr2, eqns) <- go expr1
           let rhs = Lus.SelectElement expr2
           pure (rhs, eqns)
      Lus.SelectSlice (Lus.ArraySlice start end mbStep) ->
        do (start1, eqns1) <- go start
           (end1, eqns2)   <- go end
           (step1, eqns3)  <- case mbStep of
                                Nothing  -> pure (Nothing, [])
                                Just stp -> do (stp1, eqns4) <- go stp
                                               pure (Just stp1, eqns4)
           pure ( Lus.SelectSlice (Lus.ArraySlice start1 end1 step1)
                , eqns1 ++ eqns2 ++ eqns3
                )

--------------------------------------------------------------------------------

traversePrg :: (Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation]))
            -> [Lus.TopDecl] -> PassM [Lus.TopDecl]
traversePrg f prg = traverse goDecl prg
  where
    goDecl :: Lus.TopDecl -> PassM Lus.TopDecl
    goDecl decl = case decl of
      Lus.DeclareType{}  -> pure decl
      Lus.DeclareConst{} -> pure decl
      Lus.DeclareNode nd -> Lus.DeclareNode <$> goNode nd
      Lus.DeclareNodeInst{} -> reportError (Other "normalize: unexpected DeclareNodeInst")
      Lus.DeclareContract{} -> reportError (Other "normalize: unexpected DeclareContract")

    goNode :: Lus.NodeDecl -> PassM Lus.NodeDecl
    goNode nd =
      case Lus.nodeDef nd of
        Nothing -> pure nd
        Just (Lus.NodeBody locals eqns) ->
          do let mEqns = concat <$> mapM (goEqn nd) eqns
             (eqns1, st) <- runM mEqns (NodeState mempty)
             let newLocals = map Lus.LocalVar (nLocals st)
             pure (nd { Lus.nodeDef = Just (Lus.NodeBody (locals ++ newLocals) eqns1) })

    goEqn nd eqn = case eqn of
      Lus.Define lhs rhs ->
        do (rhs1, more) <- f nd rhs
           pure ((Lus.Define lhs rhs1) : more)
      _ -> pure [eqn]

foldOnExprs :: (Lus.NodeDecl -> expr -> M (expr, [eqn]))
            -> Lus.NodeDecl -> [expr] -> M ([expr], [eqn])
foldOnExprs f nd exprs =
  foldM (\(accExprs,accEqns) expr ->
           do (expr1,eqns) <- f nd expr
              pure $ (accExprs ++ [expr1], accEqns ++ eqns))
        ([],[])
        exprs
