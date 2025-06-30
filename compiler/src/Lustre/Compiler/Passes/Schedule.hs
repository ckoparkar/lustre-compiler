module Lustre.Compiler.Passes.Schedule
  ( scheduleM ) where

import Data.List ( partition )
import Data.Set qualified as Set
import Data.Graph qualified as Graph
import Lustre.Compiler.IR.Stc
import Lustre.Compiler.Monad
import Lustre.Utils ( todo )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP

--------------------------------------------------------------------------------

scheduleM :: Program -> PassM Program
scheduleM = traverse scheduleSystem

scheduleSystem :: SystemDecl -> PassM SystemDecl
scheduleSystem s =
  do tcs <- reorderTcs (sysTcs s)
     pure s { sysTcs = tcs }

reorderTcs :: [Tc] -> PassM [Tc]
reorderTcs tcs = (locals_writeBeforeRead tcs) >>= (pure . state_readBeforeWrite)

locals_writeBeforeRead :: [Tc] -> PassM [Tc]
locals_writeBeforeRead tcs =
  let edges = map (\tc -> let binds = bindsOfTc tc
                              key = lhsVar (head binds)
                          in (tc, key , Set.toList (dependsOn tc)))
                  tcs
      (graph, vFn, _) = Graph.graphFromEdges edges
      cycles = foldr (\scc acc -> case scc of
                         Graph.CyclicSCC ls -> (PP.tupled $ map (pretty . bindsOfTc) ls) : acc
                         _ -> acc)
                     []
                     (Graph.stronglyConnComp edges)
  in case cycles of
       []  -> pure $ map (fst3 . vFn) (Graph.reverseTopSort graph)
       x:_ -> reportError (Other $ "Cyclic definitions: " ++ show x)
  where
    fst3 (a,_,_) = a


state_readBeforeWrite :: [Tc] -> [Tc]
state_readBeforeWrite tcs = let (nexts, oth) = partition isNext tcs
                            in oth ++ nexts
  where
    isNext tc = case tc of
        Next _ _ _ -> True
        _          -> False

_wellScheduled :: [Tc] -> Bool
_wellScheduled ls = todo ls

--------------------------------------------------------------------------------

class FreeVars a => DependsOn a where
  dependsOn :: a -> Set.Set CompName
  dependsOn = freeVars

instance DependsOn Tc where
  dependsOn tc = case tc of
    Next{} -> Set.empty
    _      -> freeVars tc
