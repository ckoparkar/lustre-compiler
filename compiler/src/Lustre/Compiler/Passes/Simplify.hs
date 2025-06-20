module Lustre.Compiler.Passes.Simplify
  ( simplifyM ) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Lustre.Compiler.IR.NLustre
import Lustre.Compiler.Monad ( PassM )

--------------------------------------------------------------------------------

simplifyM :: Program -> PassM Program
simplifyM p0 = foldExprsM p0 >>= elimDeadCodeM

--------------------------------------------------------------------------------

foldExprsM :: Program -> PassM Program
foldExprsM = pure . foldExprs

foldExprs :: Program -> Program
foldExprs = fmap foldExprsInNode

foldExprsInNode :: NodeDecl -> NodeDecl
foldExprsInNode (NodeDecl name binders eqns0) =
  NodeDecl name binders (runN 100 (eqns0, Map.empty))
  where
    runN :: Int -> ([Equation], Map.Map CompName Expr) -> [Equation]
    runN 0 (eqns, _env) = eqns
    runN n (eqns, env)  = runN (n-1) (run env eqns)

    run env eqns =
      foldr (\eqn (acc, accEnv) ->
               let (eqn1, env1) = go accEnv eqn
               in (eqn1 : acc, env1))
            ([], env)
            eqns

    go env eqn@(Define lhs rhs) =
      case (lhs, rhs) of
        ([LVar x], CExpr (Expr (Atom (Var nm)))) ->
          case Map.lookup nm env of
            Nothing -> (eqn, Map.insert x (Atom (Var nm)) env)
            Just r  -> (Define lhs (CExpr (Expr r)), env)
        ([LVar x], CExpr cexpr) -> case cexpr of
          Expr e   -> let e1 = goExpr env e
                      in (Define [LVar x] (CExpr (Expr e1)), Map.insert x e1 env)
          _ -> (eqn, env)
        _ -> (eqn, env)

    goExpr env expr = case expr of
      Atom a                   -> case a of
                                    Lit{} -> expr
                                    Var x -> case Map.lookup x env of
                                               Nothing -> expr
                                               Just r  -> r
      Tuple ls                 -> Tuple (map (goExpr env) ls)
      Array ls                 -> Array (map (goExpr env) ls)
      Struct tyname ls         -> Struct tyname (map (fmap (goExpr env)) ls)
      UpdateStruct tyname s ls -> UpdateStruct tyname s (map (fmap (goExpr env)) ls)
      CallPrim pr ls           -> CallPrim pr (map (goExpr env) ls)
      Select a sel             -> Select a (fmap (goExpr env) sel)
      When a b                 -> When (goExpr env a) (goExpr env b)

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
