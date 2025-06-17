module Lustre.Compiler.IR.Obc.Syntax
  ( Program, ClassDecl(..), Method(..), Stmt(..), Expr(..)
  , module Lustre.Compiler.IR.Base
  , seqStmts
  ) where

import Lustre.Compiler.IR.Base ( BaseProgram(..), BaseTopDecl(..), NodeBinders(..)
                               , Binder(..), Literal(..), Type(..), CType(..)
                               , Name(..), LHS(..), Ident(..)
                               , PrimNode(..), Op1(..), Op2(..) )

import Lustre.Compiler.IR.Base qualified as B

import Prettyprinter qualified as PP
import Prettyprinter ( Pretty(..) )
import Data.Text ( Text )

--------------------------------------------------------------------------------

type Program = BaseProgram ClassDecl

data ClassDecl = ClassDecl
  { clsName      :: Text
  , clsMemories  :: [Binder]
  , clsInstances :: [(Text, Text)]
  , clsMethods   :: [Method]
  }
  deriving Show

data Method = Method
  { mName    :: Text
  , mBinders :: NodeBinders
  , mBody    :: Stmt
  }
  deriving Show

data Stmt
  = Let (LHS B.Atom) Expr         {-^ Update       -}
  | LetState (LHS B.Atom) Expr    {-^ Update state -}
  | LetCall                       {-^ Method call  -}
      { lcBinds :: [LHS B.Atom]
      , lcClass :: (Name, Ident)
      , lcRator :: Text
      , lcRands :: [Expr]
      }
  | If Expr Stmt Stmt             {-^ Conditional  -}
  | Do Stmt Stmt                  {-^ Sequencing   -}
  | Skip                          {-^ Do nothing   -}
  deriving Show

data Expr
  = Lit Literal              {-^ Constant   -}
  | Var Name                 {-^ Read var   -}
  | SVar Name                {-^ Read state -}
  | CallPrim PrimNode [Expr] {-^ Primitives -}
  deriving Show

seqStmts :: [Stmt] -> Stmt
seqStmts = foldr Do Skip

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty ClassDecl where
  pretty cls = PP.vsep [ pretty "class" PP.<+> pretty (clsName cls) PP.<> PP.lbrace
                       , PP.indent 4 $ PP.vsep
                         [ PP.vsep (map (\(x,y) -> pretty "instance" PP.<+> pretty x PP.<> PP.colon PP.<>
                                                   pretty y PP.<> PP.semi)
                                        (clsInstances cls))
                         , PP.vsep (map (\(Binder x cty) -> pretty "memory" PP.<+> pretty x PP.<> PP.colon PP.<>
                                                            pretty (cType cty) PP.<> PP.semi)
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
    Let x rhs -> pretty x PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetState x rhs -> pretty "state" PP.<> PP.parens (pretty x) PP.<+> pretty ":=" PP.<+> pretty rhs PP.<> PP.semi
    LetCall lhs (cls,ann) f args -> PP.vsep [ pretty lhs PP.<+> PP.equals
                                            , PP.indent 4 $
                                              (pretty cls PP.<> PP.parens (pretty ann) PP.<> PP.dot PP.<>
                                               pretty f PP.<> pretty args) PP.<> PP.semi
                                            ]
    If cnd thn els -> PP.vsep [ pretty "if" PP.<+> PP.parens (pretty cnd) PP.<+> PP.lbrace
                              , PP.braces (pretty thn)
                              , PP.braces (pretty els)
                              ]
    Do s1 s2 -> PP.vsep [ pretty s1, pretty s2 ]
    Skip -> PP.emptyDoc

  prettyList stmts = PP.vsep (PP.punctuate PP.line (map pretty stmts))

instance Pretty Expr where
  pretty expr = case expr of
    Lit c  -> pretty c
    Var x  -> pretty x
    SVar x -> pretty "state" PP.<> PP.parens (pretty x)
    CallPrim pr args -> pretty pr PP.<> pretty args
  prettyList exprs = PP.tupled (map pretty exprs)
