module Lustre.Compiler.Passes.Simplify
  ( simplifyM ) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text ( pack )
import Control.Monad ( foldM )
import Control.Monad.State qualified as St
import Lustre.Compiler.IR.NLustre
import Lustre.Compiler.Monad ( PassM )

--------------------------------------------------------------------------------

simplifyM :: Program -> PassM Program
simplifyM p0 = foldExprsM p0 >>= elimDeadCodeM >>= cseM

--------------------------------------------------------------------------------
-- Expression folding
--------------------------------------------------------------------------------

foldExprsM :: Program -> PassM Program
foldExprsM = traverse (pure . foldExprsInNode)

foldExprsInNode :: NodeDecl -> NodeDecl
foldExprsInNode (NodeDecl name binders eqns0) =
  NodeDecl name binders (runN 100 (eqns0, Map.empty, Map.empty))
  where
    runN :: Int -> ([Equation], Map.Map CompName Expr, Map.Map CompName CExpr)
         -> [Equation]
    runN 0 (eqns, _eenv, _cenv) = eqns
    runN n (eqns, eenv, cenv)   = runN (n-1) (run eenv cenv eqns)

    run eenv cenv eqns =
      foldr (\eqn (acc, accEEnv, accCEnv) ->
               let (eqn1, eenv1, cenv1) = go accEEnv accCEnv eqn
               in (eqn1 : acc, eenv1, cenv1))
            ([], eenv, cenv)
            eqns

    go eenv cenv eqn@(Define lhs rhs) =
      case (lhs, rhs) of
        ([LVar x], CExpr (Expr (Atom (Var nm)))) ->
          case Map.lookup nm eenv of
            Nothing -> case Map.lookup nm cenv of
                         Nothing -> ( eqn
                                    , Map.insert x (Atom (Var nm)) eenv
                                    , Map.insert x (Expr (Atom (Var nm))) cenv
                                    )
                         Just r  -> (Define [LVar x] (CExpr r), eenv, cenv)
            Just r  -> (Define lhs (CExpr (Expr r)), eenv, cenv)
        ([LVar x], CExpr cexpr) -> case cexpr of
          Expr e   -> let e1 = goExpr eenv e
                      in (Define lhs (CExpr (Expr e1)), Map.insert x e1 eenv, cenv)
          Merge cnd alts -> let alts1 = map (\(c,e) -> let e1 = goExpr eenv e in (c, e1)) alts
                            in (Define lhs (CExpr (Merge cnd alts1)), eenv, cenv)
          If cnd thn els -> let cnd1 = goExpr eenv cnd
                                thn1 = goExpr eenv thn
                                els1 = goExpr eenv els
                                ce   = If cnd1 thn1 els1
                            in (Define lhs (CExpr ce), eenv, Map.insert x ce cenv)
        (_, Fby2 c expr) ->
          let expr1 = goExpr eenv expr
          in (Define lhs (Fby2 c expr1), eenv, cenv)
        (_, Call f args tys) ->
          let args1 = map (goExpr eenv) args
          in (Define lhs (Call f args1 tys), eenv, cenv)
        _ -> (eqn, eenv, cenv)

    goExpr env expr = case expr of
      Atom a                   -> case a of
                                    Lit{} -> expr
                                    Var x -> case Map.lookup x env of
                                               Nothing -> expr
                                               Just r  -> if needsAlloc r then expr else r
      Tuple ls                 -> Tuple (map (goExpr env) ls)
      Array ls                 -> Array (map (goExpr env) ls)
      Struct tyname ls         -> Struct tyname (map (fmap (goExpr env)) ls)
      UpdateStruct tyname s ls -> UpdateStruct tyname s (map (fmap (goExpr env)) ls)
      CallPrim pr ls ty        -> CallPrim pr (map (goExpr env) ls) ty
      Select a sel             -> Select a (fmap (goExpr env) sel)
      When a b                 -> When (goExpr env a) (goExpr env b)



needsAlloc :: Expr -> Bool
needsAlloc expr = case expr of
  Tuple{}        -> True
  Array{}        -> True
  Struct{}       -> True
  UpdateStruct{} -> True
  _ -> False

--------------------------------------------------------------------------------
-- Dead code elimination
--------------------------------------------------------------------------------

elimDeadCodeM :: Program -> PassM Program
elimDeadCodeM = pure . elimDeadCode

elimDeadCode :: Program -> Program
elimDeadCode = fmap elimDeadCodeInNode

elimDeadCodeInNode :: NodeDecl -> NodeDecl
elimDeadCodeInNode (NodeDecl name nodebinds eqns) =
  let freeInRHS = (foldr (\(Define _ rhs) acc -> freeVars rhs <> acc) Set.empty eqns) <>
                  (Set.fromList $ map binderDefines (nodeOutputs nodebinds))
      eqns1     = foldr (\eqn@(Define lhs _rhs) acc ->
                           case lhs of
                             [LVar x] -> if Set.member x freeInRHS then (eqn:acc) else acc
                             _ -> eqn : acc)
                        []
                        eqns
      fbinders bs = foldr (\b acc -> if Set.member (binderDefines b) freeInRHS then (b:acc) else acc)
                          [] bs
      nodebinds1  = NodeBinders (fbinders (nodeInputs nodebinds))
                                (fbinders (nodeOutputs nodebinds))
                                (fbinders (nodeLocals nodebinds))

  in NodeDecl name nodebinds1 eqns1


--------------------------------------------------------------------------------
-- Common sub-expression elimination
--------------------------------------------------------------------------------

cseM :: Program -> PassM Program
cseM prg = traverse (cseInNode (typeDeclsInPrg prg)) prg

data NodeState = NodeState
  { nLocals :: [Binder CType]
  , nCSE    :: Map.Map Expr CompName
  }
  deriving Show

instance Semigroup NodeState where
  (NodeState a b) <> (NodeState z y) = NodeState (a <> z) (b <> y)

instance Monoid NodeState where
  mempty = NodeState mempty mempty

type M a = St.StateT NodeState PassM a

runM :: M a -> NodeState -> PassM (a, NodeState)
runM = St.runStateT

addLocal :: CompName -> CType -> M ()
addLocal x ty =
  St.modify (\ns -> ns { nLocals = (Binder x ty) : (nLocals ns) })

cseInNode :: [TypeDecl] -> NodeDecl -> PassM NodeDecl
cseInNode tyDecls nd@(NodeDecl name binders eqns0) =
  do eqns1 <- fst <$> runM (runN 100 eqns0) (NodeState mempty mempty)
     pure (NodeDecl name binders eqns1)
  where
    runN :: Int -> [Equation] -> M [Equation]
    runN 0 eqns = pure eqns
    runN n eqns = do run1 <- run eqns
                     runN (n-1) run1

    run eqns =
      foldM (\acc eqn ->
               do eqn1 <- goRHS eqn
                  pure (eqn1 : acc))
            []
            eqns

    goRHS eqn@(Define lhs rhs) =
      case (lhs,rhs) of
        ([LVar x], CExpr (Expr expr)) ->
          do st <- St.get
             case Map.lookup expr (nCSE st) of
               Nothing -> do St.put (st { nCSE = Map.insert expr x (nCSE st) })
                             pure eqn
               Just y  -> if x /= y
                          then pure (Define lhs (CExpr (Expr (Atom (Var y)))))
                          else pure eqn
        (_, Fby2 c expr) ->
          do expr1 <- goExpr expr
             pure (Define lhs (Fby2 c expr1))
        (_, Call f ls tys) ->
          do ls1 <- mapM goExpr ls
             pure (Define lhs (Call f ls1 tys))
        _ -> pure eqn

    goExpr :: Expr -> M Expr
    goExpr expr =
      do st <- St.get
         case Map.lookup expr (nCSE st) of
           Just x  -> pure $ Atom (Var x)
           Nothing -> if needsAlloc expr
                      then do nm <- mkCompName (pack "_x") Nothing AVal
                              let [ty] = typeOf tyDecls (nodeEnv nd) expr
                              addLocal nm ty
                              St.put (st { nCSE = Map.insert expr nm (nCSE st) })
                              pure expr
                      else pure expr
