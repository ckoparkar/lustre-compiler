module Lustre.Compiler.IR.Stc.Syntax
  ( Program, SystemDecl(..), Tc(..), NodeInstInfo(..)
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
  , sysInits     :: [(CompName, Literal, Type)]
  , sysInstances :: [NodeInstInfo]
  }
  deriving Show

-- | Transition constraints.
data Tc
  = Define (LHS Expr) Clock CExpr  {-^ Basic -}
  | Next (LHS Expr) Clock Expr     {-^ Next  -}
  | Call                           {-^ Function call -}
      { cBinds :: [LHS Expr]
      , cClk   :: Clock
      , cFnName:: CompName
      , cArgs  :: [Expr]
      , cInst  :: CompName
      , cRet   :: [CType]
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

-- | Uniquely identify a node instantiation.
data NodeInstInfo = NodeInstInfo
  { niNodeName :: CompName
  , niInstName :: CompName
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty NodeInstInfo where
  pretty (NodeInstInfo node inst) = pretty node PP.<> pretty "." PP.<> pretty inst

instance Pretty SystemDecl where
  pretty sys = PP.vsep [ pretty "system" PP.<+> pretty (sysName sys) PP.<+> PP.lbrace
                       , PP.indent 4 $ PP.vsep
                         [ PP.vsep (map (\x -> pretty "instance" PP.<+> pretty x PP.<> PP.semi) (sysInstances sys))
                         , pretty "initialize" PP.<+>
                           PP.align (PP.vsep (map (\(x,c,ty) -> pretty x PP.<+> PP.equals PP.<+> pretty c PP.<+>
                                                                PP.colon PP.<+> pretty ty)
                                               (sysInits sys))) PP.<>
                           PP.semi
                         , pretty "step" PP.<> pretty (sysBinders sys)
                         , PP.lbrace
                         , PP.indent 4 (pretty (sysTcs sys))
                         , PP.rbrace
                         ]
                       , PP.rbrace
                       ]

  prettyList syss = PP.vsep (PP.punctuate PP.line (map pretty syss))

instance Pretty Tc where
  pretty tc = case tc of
    Define lhs clk rhs -> pretty lhs PP.<+> PP.equals PP.<> PP.brackets (pretty clk) PP.<+> pretty rhs
    Next lhs clk e  -> pretty "next " PP.<> pretty lhs PP.<+> PP.equals PP.<> PP.brackets (pretty clk) PP.<+> pretty e
    Call lhs clk f args ann _tys ->
                               pretty lhs PP.<+> PP.equals PP.<> PP.brackets (pretty clk) PP.<+>
                               (pretty f PP.<>
                                 PP.langle PP.<> pretty ann PP.<> PP.rangle PP.<>
                                 pretty args)
  prettyList tcs = PP.vsep $ map pretty tcs
