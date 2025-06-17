module Lustre.Compiler.IR.NLustre
  ( module Lustre.Compiler.IR.NLustre.Syntax
  , nodeEnv
  , module Language.Lustre.Name
  ) where

import Language.Lustre.Name ( Ident )
import Lustre.Compiler.IR.NLustre.Syntax
import Data.Map qualified as Map

--------------------------------------------------------------------------------

nodeEnv :: NodeDecl -> Map.Map Ident CType
nodeEnv nd =
  let (NodeBinders ins outs locals) = (nodeBinders nd)
  in Map.fromList $ map (\(Binder x ty) -> (x,ty)) (ins ++ outs ++ locals)
