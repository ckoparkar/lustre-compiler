module Lustre.Compiler.Passes.ToStc
  ( toStcM, toStc ) where

import Data.List ( nub )
import Data.Map qualified as Map
import Lustre.Compiler.IR.NLustre qualified as NL
import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.Monad ( PassM )
import Lustre.Utils ( todo )

--------------------------------------------------------------------------------

toStcM :: NL.Program -> PassM Stc.Program
toStcM = pure . toStc

toStc :: NL.Program -> Stc.Program
toStc = fmap nodeToSystem

nodeToSystem :: NL.NodeDecl -> Stc.SystemDecl
nodeToSystem nd@(NL.NodeDecl name binders eqns) =
  Stc.SystemDecl
    { Stc.sysName      = name
    , Stc.sysBinders   = binders
    , Stc.sysTcs       = tcs
    , Stc.sysInits     = inits
    , Stc.sysInstances = concatMap toInst tcs
    }
  where
    tcs = concatMap (eqnToTc (NL.nodeEnv nd)) eqns

    inits =
      map (\x -> case Map.lookup x initValues of
                   Nothing -> case Stc.cType ((NL.nodeEnv nd) Map.! x) of
                                Stc.IntType  -> (x, Stc.Int 0)
                                Stc.RealType -> (x, Stc.Real 0.0)
                                ty -> todo ty
                   Just c  -> (x,c))
          (nub initVars)

    initVars =
      foldr (\tc acc -> case tc of
                Stc.Next x _ _ -> (Stc.lhsVar x) : acc
                _              -> acc)
            []
            tcs

    initValues = Map.fromList (concatMap initValue eqns)

    initValue (NL.Define lhs rhs) =
      case (lhs, rhs) of
        ([x], NL.Fby2 c _) -> [(Stc.lhsVar x,c)]
        _                  -> []

    toInst tc =
      case tc of
        Stc.Call{Stc.cName,Stc.cAnn} -> [(cName, fst cAnn)]
        _                            -> []


eqnToTc :: Map.Map NL.CompName NL.CType -> NL.Equation -> [Stc.Tc]
eqnToTc env (NL.Define lhs rhs) = case (lhs, rhs) of
  ([x], NL.CExpr cexpr)  -> [ Stc.Define x (clockOf x) cexpr ]
  ([x], NL.Fby2 _ expr)  -> [ Stc.Next x (clockOf x) expr ]
  (xs, NL.Call f args t) -> [ Stc.Call { Stc.cBinds = xs
                                       , Stc.cClk   = (Stc.cClock (head t))
                                       , Stc.cName  = f
                                       , Stc.cArgs  = args
                                       , Stc.cAnn   = (var (head xs), False)
                                       , Stc.cRet   = t
                                       }
                            ]
  oth -> error $ "toStc: unexpected, " ++ show oth
  where
    var (NL.LVar x) = x
    var oth         = todo oth

    clockOf e = case e of
      NL.LVar x      -> NL.cClock (env Map.! x)
      NL.LSelect x _ -> clockOf x
