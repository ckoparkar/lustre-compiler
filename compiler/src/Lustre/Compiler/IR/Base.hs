module Lustre.Compiler.IR.Base
  ( module Lustre.Compiler.IR.Base
  , module Language.Lustre.AST
  , module Language.Lustre.Name
  ) where

import Language.Lustre.Name ( Ident(..), Name(..), Label(..) )
import Language.Lustre.AST ( TypeDecl(..), ConstDef(..)
                           , Literal(..), Type(..), LHS(..)
                           , Field(..), Selector(..), ArraySlice(..)
                           , PrimNode(..), Op1(..), Op2(..), OpN(..), Iter(..)
                           )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP
import Lustre.Compiler.IR.Lustre ()

--------------------------------------------------------------------------------

data BaseProgram a = Program [BaseTopDecl a]
  deriving Show

data BaseTopDecl a =
    DeclareType  !TypeDecl
  | DeclareConst !ConstDef
  | DeclareNode  a
    deriving Show

data BaseNodeDecl eqn = NodeDecl
  { nodeName    :: Ident
    -- ^ Node name

  , nodeBinders :: NodeBinders
    -- ^ Variables bound in a node

  , nodeEqns    :: [eqn]
    -- ^ Groups of recursive equations
  }
  deriving Show

data NodeBinders = NodeBinders
  { nodeInputs  :: [Binder]
  , nodeOutputs :: [Binder]
  , nodeLocals  :: [Binder]
  }
  deriving Show

-- | One or more equations.
data BaseEqnGroup eqn
  = NonRec eqn    -- ^ A non-recursive equation
  | Rec [eqn]     -- ^ A group of recursive equations.
  deriving Show

-- | Introduces a local variable.
data Binder = Binder
  { binderDefines :: Ident
  , binderType    :: CType
  }
  deriving Show

-- | Type on a boolean clock.
data CType = CType { cType :: Type, cClock :: Clock }
  deriving Show

-- | A boolean clock.  The base clock is always @true@.
data Clock = BaseClock | WhenTrue Atom
  deriving Show

-- | Atomic expressions.
data Atom
  = Lit Literal CType  {-^ Constants  -}
  | Var Name           {-^ Variable   -}
  deriving Show

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty e => Pretty (BaseProgram e) where
  pretty (Program fns) = prettyList fns

instance Pretty e => Pretty (BaseTopDecl e) where
  pretty td = case td of
    DeclareType dt      -> pretty dt
    DeclareConst cd     -> pretty cd PP.<> PP.semi
    DeclareNode nd      -> pretty nd
  prettyList decls = PP.vsep (PP.punctuate PP.line (map pretty decls))

instance Pretty NodeBinders where
  pretty (NodeBinders ins outs locals) =
    PP.vsep [ prettyList ins
            , pretty "returns" PP.<+> prettyList outs PP.<> PP.semi
            , pretty "var " PP.<> PP.hsep (PP.punctuate PP.comma (map pretty locals)) PP.<> PP.semi
            ]

instance Pretty e => Pretty (BaseNodeDecl e) where
  pretty nd = PP.vsep [ pretty "node" PP.<+> pretty (nodeName nd) PP.<> pretty (nodeBinders nd)
                      , pretty "let"
                      , PP.indent 4 (pretty (nodeEqns nd))
                      , pretty "tel"
                      ]

instance Pretty Binder where
  pretty (Binder x ty) = pretty x PP.<> pretty ":" PP.<> pretty ty
  prettyList binds   = PP.tupled (map pretty binds)

instance Pretty CType where
  pretty (CType ty clk) = case clk of
    BaseClock  -> pretty ty
    WhenTrue a -> pretty ty PP.<+> pretty "when" PP.<+> pretty a

instance Pretty Clock where
  pretty clk = case clk of
    BaseClock  -> pretty "base"
    WhenTrue a -> pretty "when" PP.<+> pretty a

instance Pretty Atom where
  pretty atom = case atom of
    Lit c ty -> pretty c PP.<> pretty ":" PP.<> pretty ty
    Var nm   -> pretty nm
