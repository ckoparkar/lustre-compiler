module Lustre.Compiler.Passes.ToStc
  ( toStcM, toStc ) where

import Data.Map qualified as Map
import Lustre.Compiler.IR.NLustre qualified as NL
import Lustre.Compiler.IR.Stc qualified as Stc
import Lustre.Compiler.Monad ( PassM )

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
    , Stc.sysInits     = concatMap toInit eqns
    , Stc.sysInstances = concatMap toInst tcs
    }
  where
    tcs = concatMap (eqnToTc (NL.nodeEnv nd)) eqns

    toInit (NL.Define lhs rhs) =
      case (lhs, rhs) of
        ([x], NL.Fby2 c _) -> [(x,c)]
        _                  -> []

    toInst tc =
      case tc of
        Stc.Call{Stc.cName,Stc.cAnn} -> [(cName, fst cAnn)]
        _                            -> []


eqnToTc :: Map.Map NL.CompName NL.CType -> NL.Equation -> [Stc.Tc]
eqnToTc env (NL.Define lhs rhs) = case (lhs, rhs) of
  ([x], NL.CExpr cexpr)  -> [ Stc.Define x (clockOf x) cexpr ]
  ([x], NL.Fby2 _ expr)  -> [ Stc.Next x (clockOf x) expr ]
  (xs, NL.Call f args _) -> [ Stc.Call { Stc.cBinds = xs
                                       , Stc.cClk   = undefined
                                       , Stc.cName  = f
                                       , Stc.cArgs  = args
                                       , Stc.cAnn   = (var (head xs), False)
                                       }
                            ]
  oth -> error $ "toStc: unexpected, " ++ show oth
  where
    var (NL.LVar x) = x
    var _oth        = _todo

    clockOf e = case e of
      NL.LVar x      -> NL.cClock (env Map.! x)
      NL.LSelect x _ -> clockOf x
