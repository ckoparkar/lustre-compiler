module Lustre.Compiler.IR.NLustre.Syntax
  ( Program, NodeDecl(..), Equation(..), RHS(..)
  , CExpr(..), Expr(..), Atom(..), Clock(..), CType(..)
  , nodeEnv
  , TypeOf(..)
  , module Lustre.Compiler.IR.Base
  ) where

import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP

import Lustre.Compiler.IR.Base
import Lustre.Compiler.IR.Lustre.Compat ()
import Lustre.Utils ( todo )
import Data.List ( find )
import Data.Set qualified as Set
import Data.Map qualified as Map

--------------------------------------------------------------------------------

type Program  = BaseProgram NodeDecl

data NodeDecl = NodeDecl
  { nodeName    :: CompName
    -- ^ Node name

  , nodeBinders :: NodeBinders CType
    -- ^ Variables bound in a node

  , nodeEqns    :: [Equation]
    -- ^ Groups of recursive equations
  }
  deriving Show

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
  deriving (Show, Eq, Ord)

-- | Simple expressions.
data Expr
  = Atom Atom
  | CallPrim PrimNode [Expr] CType
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
  deriving (Show, Eq, Ord)

-- | Atomic expressions.
data Atom
  = Lit Literal CType  {-^ Constant   -}
  | Var CompName       {-^ Variable   -}
  deriving (Show, Eq, Ord)

-- | A boolean clock.  The base clock is always @true@.
data Clock = BaseClock
           | WhenEq Atom Atom
  deriving (Show, Eq, Ord)

-- | Type and a clock.
data CType = CType { cType :: Type, cClock :: Clock }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

nodeEnv :: NodeDecl -> Map.Map CompName CType
nodeEnv nd =
  Map.fromList $ map (\(Binder x ty) -> (x,ty)) (allBinders (nodeBinders nd))

--------------------------------------------------------------------------------
-- Free variables
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
    CallPrim _ ls ty    -> Set.unions (map freeVars ls) <> freeVars ty
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
-- Type inference
--------------------------------------------------------------------------------

class TypeOf a where
  typeOf :: [TypeDecl] -> Map.Map CompName CType -> a -> [CType]

instance TypeOf Expr where
  typeOf tyDecls nenv expr = case expr of
    Atom a          -> typeOf tyDecls nenv a
    CallPrim _ _ ty -> [ty]
    Select e s  ->
      case s of
        SelectField f -> case typeOf tyDecls nenv e of
                           [CType (NamedType tyname) _] ->
                             let ftys = fieldTys tyname
                             in case find ((f ==) . fieldName) ftys of
                                  Nothing -> bad $ "Unknown field, " ++ show f
                                  Just ty -> [CType (fieldType ty) BaseClock]
                           oth -> bad $ "Not a named type, " ++ show oth
        oth -> todo oth
    Struct nm _ -> [CType (NamedType nm) BaseClock]
    UpdateStruct nm _ _ -> [CType (NamedType nm) BaseClock]
    _ -> todo (show expr)
    where
      fieldTys nm =
        case find (\d -> (typeName d) == nm) tyDecls of
          Nothing   -> bad $ "Unknown type " ++ show nm
          Just decl -> case typeDef decl of
                         Nothing  -> bad $ "Abstract type " ++ show nm
                         Just def -> case def of
                                         IsStruct fs  -> fs
                                         IsType alias -> bad $ "Alias " ++ show alias
                                         IsEnum e     -> bad $ "Enum " ++ show e


instance TypeOf Atom where
  typeOf _tyDecls nenv atom = case atom of
    Var x    -> [lookupLocal x]
    Lit _ ty -> [ty]
    where
      lookupLocal x = nenv Map.! x


bad :: String -> a
bad msg = error ("Unexpected " ++ msg)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty NodeDecl where
  pretty nd = PP.vsep [ pretty "node" PP.<+> pretty (nodeName nd) PP.<+> pretty (nodeBinders nd)
                      , pretty "let"
                      , PP.indent 4 (pretty (nodeEqns nd))
                      , pretty "tel"
                      ]

instance Pretty Equation where
  pretty (Define lhs rhs) = pretty lhs PP.<+> PP.equals PP.<+> (pretty rhs)
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
    CallPrim pr ls _ -> pretty pr PP.<> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty ls)))
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
