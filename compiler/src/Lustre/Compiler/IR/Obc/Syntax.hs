module Lustre.Compiler.IR.Obc.Syntax
  ( Program, ClassDecl(..), Method(..), Stmt(..), Expr(..), Atom(..), Var(..), LHS(..)
  , module Lustre.Compiler.IR.Base
  , seqStmts, varCompName
  ) where

import Lustre.Compiler.IR.Base hiding ( Atom(..), LHS(..) )
import Prettyprinter qualified as PP
import Prettyprinter ( Pretty(..) )

--------------------------------------------------------------------------------

type Program = BaseProgram ClassDecl

data ClassDecl = ClassDecl
  { clsName      :: CompName
  , clsMemories  :: [Binder Type]
  , clsInstances :: [(CompName, CompName)]
  , clsMethods   :: [Method]
  }
  deriving Show

data Method = Method
  { mName    :: CompName
  , mBinders :: NodeBinders Type
  , mBody    :: Stmt
  }
  deriving Show

data Stmt
  = Skip
    {-^ Do nothing -}

  | Do Stmt Stmt
    {-^ Sequencing -}

  | If Expr Stmt Stmt
    {-^ Conditional -}

  | UpdateFields
      { ufOf      :: LHS Atom
      , ufUpdates :: [Field Atom]
      }
    {-^ Update fields of a struct -}

  | Let (LHS Atom) Expr
    {-^ Assign local -}

  | LetState (LHS Atom) Expr
    {-^ Assign state -}

  | LetCopyStruct
      { lcsTo     :: LHS Atom
      , lcsFrom   :: Var
      , lcsTyName :: CompName
      }
    {-^ Copy struct -}

  | LetAllocStruct
      { lasTo     :: LHS Atom
      , lasTyName :: CompName
      }

  | LetCall
      { lcBinds :: [LHS Atom]
      , lcClass :: (CompName, CompName)
      , lcRator :: CompName
      , lcRands :: [Expr]
      }
    {- ^ Method call -}
  deriving Show

data Expr
  = Atom Atom
  | CallPrim PrimNode [Atom]
  | Select Atom (Selector Atom)
  deriving Show

data Atom
  = Lit Literal
  | Var Var
  deriving Show

data Var
  = State CompName  {-^ State -}
  | Out CompName    {-^ Output -}
  | Oth CompName    {-^ Other -}
  | Addr Var        {-^ Address of -}
  deriving Show

data LHS e
  = LVar Var
  | LSelect (LHS e) (Selector e)
  deriving Show

instance Functor LHS where
  fmap f lhs = case lhs of
    LVar x -> LVar x
    LSelect lhs1 sel -> LSelect (fmap f lhs1) (fmap f sel)

seqStmts :: [Stmt] -> Stmt
seqStmts = foldr Do Skip

varCompName :: Var -> CompName
varCompName v = case v of
  State x -> x
  Out x   -> x
  Oth x   -> x
  Addr x  -> varCompName x

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty ClassDecl where
  pretty cls = PP.vsep [ pretty "class" PP.<+> pretty (clsName cls) PP.<+> PP.lbrace
                       , PP.indent 4 $ PP.vsep
                         [ PP.vsep (map (\(x,y) -> pretty "instance" PP.<+> pretty x PP.<> PP.colon PP.<>
                                                   pretty y PP.<> PP.semi)
                                        (clsInstances cls))
                         , PP.vsep (map (\(Binder x ty) -> pretty "memory" PP.<+> pretty x PP.<> PP.colon PP.<>
                                                           pretty ty PP.<> PP.semi)
                                        (clsMemories cls))
                         , PP.line
                         , prettyList (clsMethods cls)
                         ]
                       , PP.rbrace
                       ]
  prettyList clss = PP.vsep (PP.punctuate PP.line (map pretty clss))

instance Pretty Method where
  pretty (Method nm binds body) = PP.vsep [ pretty nm PP.<> pretty binds PP.<+> PP.lbrace
                                          , PP.indent 4 (pretty body)
                                          , PP.rbrace
                                          ]
  prettyList mthds = PP.vsep (PP.punctuate PP.line (map pretty mthds))

instance Pretty Stmt where
  pretty stmt = case stmt of
    Skip -> PP.emptyDoc
    Do s1 s2 -> PP.vsep [ pretty s1, pretty s2 ]
    If cnd thn els -> PP.vsep [ pretty "if" PP.<+> PP.parens (pretty cnd) PP.<+> PP.lbrace
                              , PP.braces (pretty thn)
                              , PP.braces (pretty els)
                              ]
    UpdateFields x fs -> pretty x PP.<+> PP.braces (PP.hcat (PP.punctuate PP.semi (map pretty fs))) PP.<> PP.semi
    LetCopyStruct to from _ty -> pretty to PP.<+> pretty ":=" PP.<+> pretty "copy" PP.<+> pretty from PP.<> PP.semi
    LetAllocStruct to ty -> pretty ty PP.<+> pretty to PP.<> PP.semi
    Let x rhs -> pretty x PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetState x rhs -> pretty "state" PP.<> PP.parens (pretty x) PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetCall lhs (cls,ann) f args -> PP.vsep [ pretty lhs PP.<+> pretty ":="
                                            , PP.indent 4 $
                                              (pretty cls PP.<> PP.parens (pretty ann) PP.<> PP.dot PP.<>
                                               pretty f PP.<> pretty args) PP.<> PP.semi
                                            ]

  prettyList stmts = PP.vsep (PP.punctuate PP.line (map pretty stmts))

instance Pretty Expr where
  pretty expr = case expr of
    Atom a -> pretty a
    CallPrim pr args -> pretty pr PP.<> pretty args
    Select e s -> pretty e PP.<> pretty s
  prettyList exprs = PP.tupled (map pretty exprs)

instance Pretty Atom where
  pretty atom = case atom of
    Lit c  -> pretty c
    Var v  -> pretty v

instance Pretty Var where
  pretty v = case v of
    State x -> pretty "state" PP.<> PP.parens (pretty x)
    Out x   -> pretty "out" PP.<> PP.parens (pretty x)
    Oth x   -> pretty x
    Addr x  -> pretty "addr" PP.<> PP.parens (pretty x)

instance Pretty e => Pretty (LHS e) where
  pretty lhs = case lhs of
    LVar x      -> pretty x
    LSelect l s -> pretty l <> pretty s
