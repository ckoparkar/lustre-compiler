module Lustre.Compiler.IR.NLustre.Syntax
  ( Program, NodeDecl
  , Equation(..), RHS(..)
  , CExpr(..), Expr(..), Atom(..)
  , Clock(..), CType(..)
  , module Lustre.Compiler.IR.Base
  ) where

import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP

import Lustre.Compiler.IR.Base
import Lustre.Compiler.IR.Lustre.Compat ()
import Data.Set qualified as Set

--------------------------------------------------------------------------------

type Program  = BaseProgram NodeDecl
type NodeDecl = BaseNodeDecl Equation CType

data Equation = Define [LHS Expr] RHS
  deriving Show

-- | Equations.
data RHS
  = CExpr CExpr                     {-^ Definition    -}
  | Fby2 Literal Expr               {-^ Unit delay    -}
  | Call CompName [Expr] [CType]    {-^ Function call -}
  deriving Show

-- | Control expressions.
data CExpr
  = Expr Expr
    {-^ Simple expressions -}
  | Merge (CompName, CType) [(Literal, Expr)]
    {-^ Oversampling -}
  | If Expr Expr Expr
    {-^ Multiplexing -}
  deriving Show

-- | Simple expressions.
data Expr
  = Atom Atom
  | CallPrim PrimNode [Expr]
    {-^ Primitives -}
  | Select Atom (Selector Expr)
    {-^ Create a new struct value.  'Name' is the struct type -}
  | Tuple ![Expr]
  | Array ![Expr]
  | Struct CompName [Field Expr]
  | UpdateStruct CompName Atom [Field Expr]
    {-^ Update a struct.
      The 'Name' is the struct type.
      The expression is the struct being updated. -}
  | Expr `When` Expr
    {-^ Subsampling -}
  deriving Show

-- | Atomic expressions.
data Atom
  = Lit Literal CType  {-^ Constant   -}
  | Var CompName       {-^ Variable   -}
  deriving Show

-- | A boolean clock.  The base clock is always @true@.
data Clock = BaseClock
           | WhenEq Atom Atom
  deriving Show

-- | Type on a boolean clock.
data CType = CType { cType :: Type, cClock :: Clock }
  deriving Show

--------------------------------------------------------------------------------

instance FreeVars RHS where
  freeVars rhs = case rhs of
    CExpr ce      -> freeVars ce
    Fby2 _ e      -> freeVars e
    Call _ ls tys -> Set.unions (map freeVars ls ++ map freeVars tys)

instance FreeVars CExpr where
  freeVars expr = case expr of
    Expr e            -> freeVars e
    Merge (x, _) alts -> Set.singleton x <> Set.unions (map (\(_,e) -> freeVars e) alts)
    If cnd thn els    -> freeVars cnd <> freeVars thn <> freeVars els

instance FreeVars Expr where
  freeVars expr = case expr of
    Atom a              -> freeVars a
    CallPrim _ ls       -> Set.unions (map freeVars ls)
    Select a sel        -> freeVars a <> freeVars sel
    Tuple ls            -> Set.unions (map freeVars ls)
    Array ls            -> Set.unions (map freeVars ls)
    Struct _ ls         -> Set.unions (map freeVars ls)
    UpdateStruct _ a ls -> freeVars a <> Set.unions (map freeVars ls)
    When e1 e2          -> freeVars e1 <> freeVars e2

instance FreeVars CType where
  freeVars (CType _ clk) = freeVars clk

instance FreeVars Clock where
  freeVars clk = case clk of
    BaseClock  -> Set.empty
    WhenEq a b -> freeVars a <> freeVars b

instance FreeVars Atom where
  freeVars a = case a of
    Lit _ ty -> freeVars ty
    Var x    -> Set.singleton x

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty Equation where
  pretty (Define lhs rhs) =
    PP.vsep [ pretty lhs PP.<+> PP.equals
            , PP.indent 4 (pretty rhs)
            ]
  prettyList eqns = PP.vsep (map pretty eqns)

instance Pretty RHS where
  pretty rhs = case rhs of
    CExpr cexpr -> pretty cexpr
    Fby2 c e    -> PP.hsep [ pretty c, pretty "->", pretty e]
    Call f args _tys -> pretty f PP.<> pretty args

instance Pretty CExpr where
  pretty cexpr = case cexpr of
    Expr e -> pretty e
    Merge (a, ty) bs ->
      pretty "merge" PP.<+> pretty a PP.<> PP.line PP.<> PP.indent 4 (PP.vcat (ppBranch <$> bs))
      where
        ppBranch (lit, body) =
          pretty (Lit lit ty) PP.<+> pretty "=>" PP.<+> pretty body
    If cnd thn els  -> PP.vsep [ pretty "if" PP.<+> pretty cnd, pretty thn, pretty els ]

instance Pretty Expr where
  pretty expr = case expr of
    Atom c -> pretty c
    CallPrim pr ls -> pretty pr PP.<> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty ls)))
    Select e s    -> pretty e PP.<> pretty s
    Tuple es      -> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Array es      -> PP.brackets (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Struct s fs   -> pretty s PP.<+> PP.braces (PP.align (PP.vcat (PP.punctuate PP.semi (map pretty fs))))
    UpdateStruct s x fs ->
      pretty s PP.<+> PP.braces (pretty x PP.<+> pretty "with" PP.<+>
                                 PP.align (PP.vcat (PP.punctuate PP.semi (map pretty fs))))
    When e b    -> pretty e PP.<+> pretty "when" PP.<+> pretty b
  prettyList exprs = PP.tupled (map pretty exprs)

instance Pretty CType where
  pretty (CType ty clk) = case clk of
    BaseClock  -> pretty ty
    WhenEq n l -> pretty n PP.<+> pretty "=" PP.<+> pretty l

instance Pretty Clock where
  pretty clk = case clk of
    BaseClock  -> pretty "base"
    WhenEq n l -> pretty "wheneq" PP.<+> PP.parens (PP.hsep [pretty n, pretty l])

instance Pretty Atom where
  pretty atom = case atom of
    Lit c ty -> pretty c PP.<> pretty ":" PP.<> pretty ty
    Var nm   -> pretty nm
