module Lustre.Compiler.Passes.Schedule
  ( scheduleM, schedule ) where

import Data.List ( partition )
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
reorderTcs = readStateBeforeWrite
  where
    isNext tc = case tc of
      Next _ _ _ -> True
      _          -> False

    readStateBeforeWrite tcs =
      let (nexts, oth) = partition isNext tcs
      in oth ++ nexts

_wellScheduled :: [Tc] -> Bool
_wellScheduled ls = todo ls
