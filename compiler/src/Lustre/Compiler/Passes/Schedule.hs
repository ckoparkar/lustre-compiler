module Lustre.Compiler.Passes.Schedule
  ( scheduleM, schedule ) where

import Data.List ( partition )
import Data.Set qualified as Set
import Data.Graph qualified as Graph
import Lustre.Compiler.IR.Stc
import Lustre.Compiler.Monad ( PassM )
import Lustre.Utils ( todo )

--------------------------------------------------------------------------------

scheduleM :: Program -> PassM Program
scheduleM = pure . schedule

schedule :: Program -> Program
schedule (Program ls) = Program (map go ls)
  where
    go decl = case decl of
      DeclareNode s -> DeclareNode (scheduleSystem s)
      _             -> decl

scheduleSystem :: SystemDecl -> SystemDecl
scheduleSystem s = s { sysTcs = reorderTcs (sysTcs s) }

reorderTcs :: [Tc] -> [Tc]
reorderTcs = state_readBeforeWrite . locals_writeBeforeRead

locals_writeBeforeRead :: [Tc] -> [Tc]
locals_writeBeforeRead tcs =
  let edges           = map (\tc -> let binds = bindsOfTc tc
                                        key = lhsKey (head binds)
                                    in (tc, key , Set.toList (freeVars tc)))
                            tcs
      (graph, vFn, _) = Graph.graphFromEdges edges
      sorted          = Graph.reverseTopSort graph
  in map (fst3 . vFn) sorted
  where
    fst3 (a,_,_) = a
    lhsKey lhs = case lhs of
                   LVar x         -> x
                   LSelect lhs1 _ -> lhsKey lhs1

state_readBeforeWrite :: [Tc] -> [Tc]
state_readBeforeWrite tcs = let (nexts, oth) = partition isNext tcs
                           in oth ++ nexts
  where
    isNext tc = case tc of
        Next _ _ _ -> True
        _          -> False

_wellScheduled :: [Tc] -> Bool
_wellScheduled ls = todo ls
