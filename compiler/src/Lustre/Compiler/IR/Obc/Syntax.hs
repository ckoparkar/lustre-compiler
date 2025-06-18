module Lustre.Compiler.IR.Obc.Syntax
  ( Program, ClassDecl(..), Method(..), Stmt(..), Expr(..), Atom(..)
  , module Lustre.Compiler.IR.Base
  , seqStmts
  ) where

import Lustre.Compiler.IR.Base hiding ( Atom(..) )
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
  = Skip                        {-^ Do nothing   -}
  | Do Stmt Stmt                {-^ Sequencing   -}
  | If Expr Stmt Stmt           {-^ Conditional  -}
  | Let (LHS Atom) Expr         {-^ Update       -}
  | LetState (LHS Atom) Expr    {-^ Update state -}
  | LetCall                     {-^ Method call  -}
      { lcBinds :: [LHS Atom]
      , lcClass :: (CompName, CompName)
      , lcRator :: CompName
      , lcRands :: [Expr]
      }
  deriving Show

data Expr
  = Atom Atom
  | CallPrim PrimNode [Atom] {-^ Primitives -}
  deriving Show

data Atom
  = Lit Literal    {-^ Constant   -}
  | Var CompName   {-^ Read local -}
  | SVar CompName  {-^ Read state -}
  deriving Show

seqStmts :: [Stmt] -> Stmt
seqStmts = foldr Do Skip

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
    Let x rhs -> pretty x PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetState x rhs -> pretty "state" PP.<> PP.parens (pretty x) PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetCall lhs (cls,ann) f args -> PP.vsep [ pretty lhs PP.<+> PP.equals
                                            , PP.indent 4 $
                                              (pretty cls PP.<> PP.parens (pretty ann) PP.<> PP.dot PP.<>
                                               pretty f PP.<> pretty args) PP.<> PP.semi
                                            ]

  prettyList stmts = PP.vsep (PP.punctuate PP.line (map pretty stmts))

instance Pretty Expr where
  pretty expr = case expr of
    Atom a -> pretty a
    CallPrim pr args -> pretty pr PP.<> pretty args
  prettyList exprs = PP.tupled (map pretty exprs)

instance Pretty Atom where
  pretty atom = case atom of
    Lit c  -> pretty c
    Var x  -> pretty x
    SVar x -> pretty "state" PP.<> PP.parens (pretty x)
