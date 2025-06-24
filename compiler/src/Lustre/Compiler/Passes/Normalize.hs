module Lustre.Compiler.Passes.Normalize
  (normalizeM) where

import Language.Lustre.AST qualified as Lus
import Language.Lustre.Name qualified as Name
import Control.Monad.State qualified as St
import Control.Monad ( foldM )
import Data.Map qualified as Map

import Lustre.Compiler.IR.Lustre.Compat
import Lustre.Compiler.IR.NLustre qualified as NL
import Lustre.Compiler.Monad
import Lustre.Utils ( todo )

import GHC.Stack ( HasCallStack )

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
      Lus.DeclareType tydecl -> pure (NL.DeclareType (toTypeDecl tydecl))
      Lus.DeclareConst cdecl -> pure (NL.DeclareConst (toConstDef cdecl))
      Lus.DeclareNode ndecl  -> pure (NL.DeclareNode (toNode enumConMap ndecl))
      Lus.DeclareNodeInst{}  -> reportError (Other "normalize: bad DeclareNodeInst")
      Lus.DeclareContract{}  -> reportError (Other "normalize: bad DeclareContract")

    toTypeDecl (Lus.TypeDecl nm def) =
      NL.TypeDecl (NL.compNameFromIdent nm) (fmap toTypeDef def)

    toTypeDef tydef = case tydef of
      Lus.IsType ty   -> NL.IsType (toType ty)
      Lus.IsEnum ls   -> NL.IsEnum (map (\i -> let x = NL.compNameFromIdent i
                                               in (x, enumConMap Map.! x))
                                        ls)
      Lus.IsStruct ls -> NL.IsStruct (map toFieldType ls)

    toFieldType (Lus.FieldType nm ty e) =
      NL.FieldType (Name.labText nm) (toType ty) (fmap (toLit enumConMap) e)

    toConstDef (Lus.ConstDef nm ty e) =
      NL.ConstDef (NL.compNameFromIdent nm) (fmap toType ty) (fmap (toLit enumConMap) e)

    enumConMap = foldr (\is acc -> Map.fromList (zip is [0..]) <> acc) Map.empty enums

    enums = [ map NL.compNameFromIdent is |
              Lus.TypeDecl { Lus.typeDef = Just (Lus.IsEnum is) } <- (typeDecls decls) ]

toType :: Lus.Type -> NL.Type
toType ty = case ty of
      Lus.TypeRange _ ty1   -> toType ty1
      Lus.IntType           -> NL.IntType
      Lus.RealType          -> NL.RealType
      Lus.BoolType          -> NL.BoolType
      Lus.NamedType nm      -> NL.NamedType (NL.compNameFromName nm)
      Lus.ArrayType{}       -> todo ty
      Lus.IntSubrange e1 e2 ->
        case (e1, e2) of
          (Lus.Lit (Lus.Int i1), Lus.Lit (Lus.Int i2)) -> NL.IntSubrange i1 i2
          _ -> bad $ "toType: IntSubrange, " ++ show (e1, e2)

toLit :: Map.Map NL.CompName Integer -> Lus.Expression -> NL.Literal
toLit enumConMap expr = case expr of
      Lus.ERange _ e -> toLit enumConMap e
      Lus.Const e _  -> toLit enumConMap e
      Lus.Lit c      -> c
      Lus.Var x      -> NL.Int (enumConMap Map.! (NL.compNameFromName x))
      _              -> bad $ "toLit: " ++ show expr

toNode :: Map.Map NL.CompName Integer -> Lus.NodeDecl -> NL.NodeDecl
toNode enumConMap nd =
      let eqns = case Lus.nodeDef nd of
                   Nothing -> []
                   Just (Lus.NodeBody _ eqs) -> concatMap toEqn eqs
          (ins, outs, locals) = nodeBinders nd
      in NL.NodeDecl (NL.compNameFromIdent (Lus.nodeName nd))
                     (NL.NodeBinders (map toBinder ins)
                                     (map toBinder outs)
                                     (map toBinder locals))
                     eqns

  where
    toBinder (Lus.Binder x ty) =
      NL.Binder (NL.compNameFromIdent x) (toCType ty)

    toCType (Lus.CType ty clk) =
      NL.CType (toType ty) (toClock clk)

    toClock clk = case clk of
      Lus.BaseClock    -> NL.BaseClock
      Lus.ClockVar{}   -> bad $ "toClock: " ++ show clk
      Lus.KnownClock e -> toClockExpr e

    toClockExpr :: HasCallStack => Lus.ClockExpr -> NL.Clock
    toClockExpr (Lus.WhenClock _ e nm) =
      case typeOf [] nd e of
        [ty] -> NL.WhenEq (toAtom e) (NL.Var (NL.compNameFromIdent nm))
        oth  -> bad $ "toClockExpr: type of clock condition, " ++ show oth

    toEqn eqn = case eqn of
      Lus.Define lhs rhs -> [NL.Define (map toLHS lhs) (toRHS rhs)]
      _                  -> []

    toLHS lhs = case lhs of
      Lus.LVar x        -> NL.LVar (NL.compNameFromIdent x)
      Lus.LSelect x sel -> NL.LSelect (toLHS x) (toSel sel)

    toRHS expr = case expr of
      Lus.ERange _ e -> toRHS e
      Lus.Call (Lus.NodeInst fn []) args _clk (Just ctys) ->
        case fn of
          Lus.CallUser nm     -> NL.Call (NL.compNameFromName nm) (map toExpr args) (map toCType ctys)
          Lus.CallPrim _ prim -> case (prim, args) of
            (Lus.Op2 Lus.Fby, [Lus.Const (Lus.Lit c) _, next]) -> NL.Fby2 c (toExpr next)
            _ -> NL.CExpr (toCExpr expr)
      _ -> NL.CExpr (toCExpr expr)

    toCExpr expr = case expr of
      Lus.Merge i alts ->
        let iName = Name.origNameToName (Name.identOrigName i)
            alts1 = map (\(Lus.MergeCase k e) -> (toLit enumConMap k, toExpr e)) alts
        in case typeOf [] nd (Lus.Var iName) of
             [ty] -> (NL.Merge (NL.compNameFromName iName, toCType ty) alts1)
             oth  -> bad $ "toCExpr: type of scrutinee, " ++ show oth
      Lus.Call (Lus.NodeInst fn []) args _clk _ctys ->
        case fn of
          Lus.CallUser{} -> bad $ "toCExpr: " ++ show expr
          Lus.CallPrim _ prim -> case (prim, args) of
            (Lus.ITE, [cnd, thn, els]) -> NL.If (toExpr cnd) (toExpr thn) (toExpr els)
            _ -> NL.Expr (toExpr expr)
      _ -> NL.Expr (toExpr expr)

    toExpr expr = case expr of
      Lus.ERange _ e -> toExpr e
      Lus.Var{}      -> NL.Atom (toAtom expr)
      Lus.Lit{}      -> NL.Atom (toAtom expr)
      Lus.Const{}    -> NL.Atom (toAtom expr)
      Lus.When e clk -> case toClockExpr clk of
                          NL.BaseClock  ->
                            (toExpr e) `NL.When`
                            (NL.Atom (NL.Lit (NL.Bool True)
                                     (NL.CType NL.BoolType  NL.BaseClock)))
                          NL.WhenEq a b ->
                            (toExpr e) `NL.When`
                            (NL.CallPrim (NL.Op2 NL.Eq) [NL.Atom a, NL.Atom b])
      Lus.Tuple ls   -> NL.Tuple (map toExpr ls)
      Lus.Array ls   -> NL.Array (map toExpr ls)
      Lus.Select e sel         -> NL.Select (toAtom e) (toSel sel)
      Lus.Struct nm ls         -> NL.Struct (NL.compNameFromName nm) (map (fmap toExpr) (map toField ls))
      Lus.UpdateStruct Nothing _ _    -> bad $ "toExpr: " ++ show expr
      Lus.UpdateStruct (Just nm) e ls -> NL.UpdateStruct (NL.compNameFromName nm) (toAtom e) (map (fmap toExpr) (map toField ls))
      Lus.WithThenElse{}       -> todo expr
      Lus.Merge{}              -> bad $ "toExpr: " ++ show expr
      Lus.Call (Lus.NodeInst fn []) args _clk _ctys ->
        let args1 = map toExpr args in
          case fn of
            Lus.CallUser{}      -> bad $ "UntoExpr: " ++ show expr
            Lus.CallPrim _ prim -> NL.CallPrim prim args1
      Lus.Call (Lus.NodeInst _ (_x:_xs)) _args _clk __ctys ->
        bad $ "toExpr: static arguments not empty, " ++ show expr

    toField (Lus.Field lbl val) = NL.Field (Name.labText lbl) val

    toAtom expr = case expr of
      Lus.Var x      -> NL.Var (NL.compNameFromName x)
      Lus.Const c ty -> NL.Lit (toLit enumConMap c) (toCType ty)
      _ -> bad $ "toAtom: " ++ show expr

    toSel sel = case sel of
      Lus.SelectField lbl -> NL.SelectField (Name.labText lbl)
      Lus.SelectElement e -> NL.SelectElement (toExpr e)
      Lus.SelectSlice e   -> NL.SelectSlice (fmap toExpr e)

bad :: HasCallStack => String -> a
bad msg = error ("Unexpected " ++ msg)

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

addLocal :: Name.Ident -> Lus.CType -> M ()
addLocal x ty =
  St.modify (\ns -> ns { nLocals = (Lus.Binder x ty) : (nLocals ns) })

typeOfM :: TypeOf a => [Lus.TypeDecl] -> Lus.NodeDecl -> a -> M [Lus.CType]
typeOfM tyDecls nd e =
  do st <- St.get
     let locals = map Lus.LocalVar (nLocals st)
         nd1    = bindLocals locals nd
     pure (typeOf tyDecls nd1 e)

--------------------------------------------------------------------------------

normFbyExpr :: [Lus.TypeDecl] -> Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation])
normFbyExpr _tyDecls _nd expr
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
                          _ -> reportError (Other $ "normFbyExpr: bad initial type, " ++ show ty)
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
                                            [Lus.Var (Name.Unqual xinit), e0, Lus.Var (Name.Unqual px)]
                                            clk
                                            (Just [ty]))
           pure ( Lus.Var (Name.Unqual res), [eqn1, eqn2, eqn3] )
      _ -> reportError (Other $ "normFbyExpr: not ANF, " ++ show expr)
  | otherwise
  = pure (expr, [])

toAnfExpr :: [Lus.TypeDecl] -> Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation])
toAnfExpr tyDecls nd expr = case expr of
  Lus.Lit{}   -> (pure (expr, []))
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
    do (fields1, eqns) <- foldE toAnfField tyDecls nd fields
       let rhs = Lus.Struct nm fields1
       bind rhs eqns
  Lus.UpdateStruct nm expr1 fields ->
    do (fields1, eqns1) <- foldE toAnfField tyDecls nd fields
       (expr2, eqns2) <- go expr1
       let rhs = Lus.UpdateStruct nm expr2 fields1
       bind rhs (eqns1 ++ eqns2)
  Lus.Call fn args clk ctys ->
    do (args1, eqns) <- toAnfExprs args
       let rhs = Lus.Call fn args1 clk ctys
       bind rhs eqns
  Lus.Merge nm alts ->
    do (alts1, eqns) <- foldE toAnfAlt tyDecls nd alts
       let rhs = Lus.Merge nm alts1
       bind rhs eqns
  Lus.WithThenElse{} -> reportError (Other $ "TODO: " ++ show expr)
  where
    go = toAnfExpr tyDecls nd
    toAnfExprs = foldE toAnfExpr tyDecls nd

    bind rhs eqns =
      do ident <- freshIdent
         let nm  = Name.Unqual ident
             lhs = Lus.LVar ident
             eqn =  Lus.Define [lhs] rhs
         [ty]  <- typeOfM tyDecls nd rhs
         addLocal ident ty
         pure (Lus.Var nm, eqn:eqns)

    toAnfAlt _tyDecls _nd (Lus.MergeCase pat rhs) =
      do (pat1, eqns1) <- go pat
         (rhs1, eqns2) <- go rhs
         pure (Lus.MergeCase pat1 rhs1, eqns1 ++ eqns2)

    toAnfField _tyDecls _nd (Lus.Field fnm fval) =
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

traversePrg :: ([Lus.TypeDecl] -> Lus.NodeDecl -> Lus.Expression -> M (Lus.Expression, [Lus.Equation]))
            -> [Lus.TopDecl] -> PassM [Lus.TopDecl]
traversePrg f prg = traverse goDecl prg
  where
    goDecl :: Lus.TopDecl -> PassM Lus.TopDecl
    goDecl decl = case decl of
      Lus.DeclareType{}  -> pure decl
      Lus.DeclareConst{} -> pure decl
      Lus.DeclareNode nd -> Lus.DeclareNode <$> goNode nd
      Lus.DeclareNodeInst{} -> reportError (Other "normalize: bad DeclareNodeInst")
      Lus.DeclareContract{} -> reportError (Other "normalize: bad DeclareContract")

    tyDecls = typeDecls prg

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
        do (rhs1, more) <- f tyDecls nd rhs
           pure (more ++ [Lus.Define lhs rhs1])
      _ -> pure [eqn]

foldE :: ([Lus.TypeDecl] -> Lus.NodeDecl -> expr -> M (expr, [eqn]))
      -> [Lus.TypeDecl] -> Lus.NodeDecl -> [expr] -> M ([expr], [eqn])
foldE f tyDecls nd exprs =
  foldM (\(accExprs,accEqns) expr ->
           do (expr1,eqns) <- f tyDecls nd expr
              pure $ (accExprs ++ [expr1], accEqns ++ eqns))
        ([],[])
        exprs

typeDecls :: [Lus.TopDecl] -> [Lus.TypeDecl]
typeDecls =
  foldr (\decl acc -> case decl of
            Lus.DeclareType d -> d : acc
            _ -> acc)
        []
