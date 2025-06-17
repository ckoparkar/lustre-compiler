module Lustre.Compiler.IR.NLustre.Syntax
  ( Program, NodeDecl
  , Equation(..), RHS(..)
  , CExpr(..), Expr(..), Atom(..)
  , module Language.Lustre.AST
  , module Language.Lustre.Name
  , module Lustre.Compiler.IR.Base
  ) where

import Language.Lustre.Name ( Name(..) )
import Language.Lustre.AST ( Literal(..), Type(..), LHS(..)
                           , Field(..), Selector(..), ArraySlice(..)
                           , PrimNode(..), Op1(..), Op2(..), OpN(..), Iter(..)
                           )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP

import Lustre.Compiler.IR.Base
import Lustre.Compiler.IR.Lustre ()

--------------------------------------------------------------------------------

type Program  = BaseProgram NodeDecl
type NodeDecl = BaseNodeDecl Equation

data Equation = Define [LHS Atom] RHS
  deriving Show

-- | Equations.
data RHS
  = CExpr CExpr                     {-^ Definition -}
  | Fby2 Literal Expr               {-^ Delay      -}
  | Call Name [Atom] (Maybe [CType]) {-^ Apply node -}
  deriving Show

-- | Control expressions.
data CExpr
  = Expr Expr
    {-^ Simple expressions -}
  | Merge (Name, CType) [(Literal, Atom)]
    {-^ Oversampling -}
  | If Atom Expr Expr
    {-^ Multiplexing -}
  deriving Show

-- | Simple expressions.
data Expr
  = Atom Atom
  | CallPrim PrimNode [Atom]
    {-^ Primitives -}

  | Tuple ![Atom]
  | Array ![Atom]
  | Select Atom (Selector Atom)
  | Struct Name [Field Atom]
    {-^ Create a new struct value.  'Name' is the struct type -}

  | UpdateStruct (Maybe Name) Atom [Field Atom]
    {-^ Update a struct.
      The 'Name' is the struct type.
      The expression is the struct being updated. -}

  | Atom `When` Atom
    {-^ Subsampling -}
  deriving Show

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
    Merge{} -> pretty (show cexpr)
    If cnd thn els  -> PP.vsep [ pretty "if" PP.<+> pretty cnd, pretty thn, pretty els ]

instance Pretty Expr where
  pretty expr = case expr of
    Atom c -> pretty c
    CallPrim pr ls -> pretty pr PP.<> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty ls)))
    Tuple es      -> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Array es      -> PP.brackets (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Select e s    -> pretty e PP.<> pretty s
    Struct s fs   -> pretty s PP.<+> PP.braces (PP.vcat (PP.punctuate PP.semi (map pretty fs)))
    UpdateStruct mb x fs ->
      case mb of
        Just s -> pretty s PP.<+> PP.braces (pretty x PP.<+> pretty "with" PP.<+>
                                             PP.vcat (PP.punctuate PP.semi (map pretty fs)))
        Nothing -> pretty x PP.<+> PP.hsep (map ppF fs)
          where ppF (Field name val) = PP.braces (pretty name PP.<+> pretty ":=" PP.<+> pretty val)
    When e b    -> pretty e PP.<+> pretty "when" PP.<+> pretty b
  prettyList exprs = PP.tupled (map pretty exprs)
