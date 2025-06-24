module Lustre.Compiler.IR.Stc.Syntax
  ( Program, SystemDecl(..), Tc(..)
  , bindsOfTc
  , module Lustre.Compiler.IR.NLustre.Syntax
  , module Lustre.Compiler.IR.Base
  ) where

import Data.Set qualified as Set
import Prettyprinter qualified as PP
import Prettyprinter ( Pretty(..) )

import Lustre.Compiler.IR.NLustre.Syntax ( CExpr(..), Expr(..), Atom(..)
                                         , CType(..), Clock(..) )
import Lustre.Compiler.IR.Base

--------------------------------------------------------------------------------

type Program = BaseProgram SystemDecl

data SystemDecl = SystemDecl
  { sysName      :: CompName
  , sysBinders   :: NodeBinders CType
  , sysTcs       :: [Tc]
  , sysInits     :: [(LHS Expr, Literal)]
  , sysInstances :: [(CompName, CompName)]
  }
  deriving Show

-- | Transition constraints.
data Tc
  = Define (LHS Expr) Clock CExpr  {-^ Basic -}
  | Next (LHS Expr) Clock Expr     {-^ Next  -}
  | Call                           {-^ Function call -}
      { cBinds :: [LHS Expr]
      , cClk   :: Clock
      , cName  :: CompName
      , cArgs  :: [Expr]
      , cAnn   :: (CompName, Bool)
      }
  deriving Show

bindsOfTc :: Tc -> [LHS Expr]
bindsOfTc tc = case tc of
  Define lhs _ _ -> [lhs]
  Next lhs _ _   -> [lhs]
  Call{cBinds}   -> cBinds

instance FreeVars Tc where
  freeVars tc = case tc of
    Define _ clk ce  -> freeVars clk <> freeVars ce
    Next _ clk e     -> freeVars clk <> freeVars e
    Call{cClk,cArgs} -> freeVars cClk <> Set.unions (map freeVars cArgs)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty SystemDecl where
  pretty sys = PP.vsep [ pretty "system" PP.<+> pretty (sysName sys) PP.<+> PP.lbrace
                       , PP.indent 4 $ PP.vsep
                         [ pretty "system" PP.<+>
                           PP.vsep (map (\(x,y) -> pretty x PP.<> pretty y) (sysInstances sys)) PP.<>
                           PP.semi
                         , pretty "init" PP.<+>
                           PP.align (PP.vsep (map (\(x,c) -> pretty x PP.<+> PP.equals PP.<+> pretty c )
                                               (sysInits sys))) PP.<>
                           PP.semi
                         , pretty "transition" PP.<+> pretty (sysBinders sys)
                         , PP.lbrace
                         , PP.indent 4 (pretty (sysTcs sys))
                         , PP.rbrace
                         ]
                       , PP.rbrace
                       ]

  prettyList syss = PP.vsep (PP.punctuate PP.line (map pretty syss))

instance Pretty Tc where
  pretty tc = case tc of
    Define lhs clk rhs -> PP.vsep [ pretty lhs PP.<+> PP.equals PP.<> PP.parens (pretty clk)
                                  , PP.indent 4 (pretty rhs)
                                  ]
    Next lhs clk e  -> PP.vsep [ pretty "next " PP.<> pretty lhs PP.<+> PP.equals PP.<> PP.parens (pretty clk)
                               , PP.indent 4 (pretty e)
                               ]
    Call lhs clk f args ann -> PP.vsep [ pretty lhs PP.<+> PP.equals PP.<> PP.parens (pretty clk)
                                       , PP.indent 4 $
                                           (pretty f PP.<>
                                            PP.langle PP.<> pretty ann PP.<> PP.rangle PP.<>
                                            pretty args)
                                       ]
  prettyList tcs = PP.vsep $ map pretty tcs
