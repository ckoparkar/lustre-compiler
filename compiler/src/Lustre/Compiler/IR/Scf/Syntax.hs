module Lustre.Compiler.IR.Scf.Syntax
  ( Program, NodeInstInfo(..)
  , ClassDecl(..), Method(..), Stmt(..), Expr(..), Atom(..), Var(..), LHS(..)
  , module Lustre.Compiler.IR.Base
  ) where

import Lustre.Compiler.IR.Stc.Syntax ( NodeInstInfo(..) )
import Lustre.Compiler.IR.Base hiding ( LHS(..) )
import Prettyprinter qualified as PP
import Prettyprinter ( Pretty(..) )

--------------------------------------------------------------------------------

type Program = BaseProgram ClassDecl

data ClassDecl = ClassDecl
  { clsName      :: CompName
  , clsInits     :: [(CompName, Literal, Type)]
  , clsInstances :: [NodeInstInfo]
  , clsMethods   :: [Method]
  }
  deriving Show

data Method = Method
  { mName    :: CompName
  , mIns     :: [Binder Type]
  , mOuts    :: [Type]
  , mBody    :: [Stmt]
  }
  deriving Show

data Stmt
  = Expr Expr
  | Declare [LHS Expr] Type
  | Define [LHS Expr] Stmt
  | Assign [LHS Expr] Stmt
  | Switch Expr [(Literal, Stmt)]
  | If Expr Stmt (Maybe Stmt)
  | Call
      { cClsName    :: CompName
      , cMethodName :: CompName
      , cArgs  :: [Expr]
      , cInst  :: CompName
      , cRet   :: [Type]
      }
  | Return [Atom]
  | Yield [Atom]
  | Do Stmt Stmt
  deriving Show

data Expr
  = Atom Atom
  | CallPrim PrimNode [Expr] Type
  | Select Atom (Selector Expr)
  | Tuple ![Expr]
  | Array ![Expr]
  | Struct CompName [Field Expr]
  | UpdateStruct CompName Atom [Field Expr]
  deriving Show

data Atom
  = Lit Literal Type
  | Var Var
  deriving Show

data Var
  = ClsMember CompName {- ^ Class field -}
  | Reg CompName   {- ^ Regular var -}
  deriving Show

data LHS e
  = LVar Var
  | LSelect (LHS e) (Selector e)
  deriving Show

instance Functor LHS where
  fmap f lhs = case lhs of
    LVar x -> LVar x
    LSelect lhs1 sel -> LSelect (fmap f lhs1) (fmap f sel)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty ClassDecl where
  pretty cls = PP.vsep [ pretty "class" PP.<+> pretty "@" PP.<> pretty (clsName cls) PP.<+> PP.lbrace
                       , PP.indent 4 $ PP.vsep
                         [ PP.vsep (map (\(NodeInstInfo n i) -> pretty "field" PP.<+>
                                                                pretty n PP.<+> pretty "@" PP.<> pretty i)
                                        (clsInstances cls))
                         , PP.vsep (map (\(x,c,ty) -> pretty "field" PP.<+> pretty "@" PP.<> pretty x PP.<+>
                                                   PP.equals PP.<+>
                                                   pretty c PP.<+> PP.colon PP.<+> pretty ty)
                                        (clsInits cls))
                         , pretty (clsMethods cls)
                         ]
                       , PP.rbrace
                       ]
  prettyList syss = PP.vsep (PP.punctuate PP.line (map pretty syss))

instance Pretty Method where
  pretty (Method nm ins outs body) =
    PP.vsep [ pretty "fn" PP.<+> pretty "@" PP.<> pretty nm PP.<+>
              PP.tupled (map (\(Binder x ty) -> pretty "%" PP.<> pretty x PP.<+> pretty ":" PP.<+> pretty ty)
                             ins) PP.<+>
              (case outs of
                [] -> mempty
                _  -> pretty "->" PP.<+> pretty outs) PP.<+> PP.lbrace
            , PP.indent 4 (pretty body)
            , PP.rbrace
            ]
  prettyList mthds = PP.vsep (PP.punctuate PP.line (map pretty mthds))

instance Pretty Stmt where
  pretty stmt = case stmt of
    Define lhs rhs -> pretty lhs PP.<+> PP.equals PP.<+> pretty rhs
    Assign lhs rhs -> pretty "assign" PP.<> PP.tupled [pretty lhs, pretty rhs]
    Declare lhs ty -> pretty "var" PP.<+> pretty lhs PP.<+> PP.colon PP.<+> pretty ty
    Return ls      -> pretty "return" PP.<+> pretty ls
    Expr e         -> pretty e
    If cnd thn els -> PP.vsep
                       ([ pretty "if" PP.<+> PP.parens (pretty cnd) PP.<+> PP.lbrace
                        , PP.indent 4 (pretty thn) ] ++
                        case els of
                          Nothing -> [ PP.rbrace ]
                          Just e  ->  [ PP.rbrace PP.<+> pretty "else" PP.<+> PP.lbrace
                                      , PP.indent 4 (pretty e)
                                      , PP.rbrace ])
    Switch cnd ls -> PP.vsep [ pretty "switch" PP.<+> PP.parens (pretty cnd) PP.<+> PP.lbrace
                             , PP.indent 4
                                 (PP.vcat (map (\(c,e) -> pretty c PP.<+> pretty "=>" PP.<+> PP.lbrace PP.<>
                                                          PP.line PP.<>
                                                          PP.vsep [ PP.indent 4 (pretty e) ]
                                                          PP.<> PP.line PP.<>
                                                          PP.rbrace
                                                          )
                                               ls))
                             , PP.rbrace
                             ]
    Call cls f args inst _retty -> pretty cls PP.<> PP.dot PP.<> pretty f PP.<>
                                   PP.brackets (pretty "@" PP.<> pretty inst) PP.<> pretty args
    Yield a -> pretty "yield" PP.<+> pretty a
    Do s1 s2 -> PP.vsep [ pretty s1, pretty s2]
  prettyList eqns = PP.vsep (map pretty eqns)


instance Pretty Expr where
  pretty expr = case expr of
    Atom c -> pretty c
    CallPrim pr ls _ -> pretty pr PP.<> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty ls)))
    Select e s    -> pretty e PP.<> pretty s
    Tuple es      -> PP.parens (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Array es      -> PP.brackets (PP.hsep (PP.punctuate PP.comma (map pretty es)))
    Struct s fs   -> pretty s PP.<+> PP.braces (PP.align (PP.vcat (PP.punctuate PP.semi (map pretty fs))))
    UpdateStruct _s x fs ->
      -- pretty s PP.<+> PP.braces (pretty x PP.<+> pretty "with" PP.<+>
      --                            PP.align (PP.vcat (PP.punctuate PP.semi (map pretty fs))))
      pretty x PP.<+> pretty "with" PP.<+>
      PP.braces (PP.align (PP.vcat (PP.punctuate PP.semi (map pretty fs))))
  prettyList ls = PP.tupled (map pretty ls)

instance Pretty Atom where
  pretty atom = case atom of
    Lit c _ty -> pretty c
    Var v -> pretty v

  prettyList ls = PP.tupled (map pretty ls)

instance Pretty Var where
  pretty v = case v of
    ClsMember x -> pretty "@" PP.<> pretty x
    Reg x       -> pretty "%" PP.<> pretty x

instance Pretty e => Pretty (LHS e) where
  pretty lhs = case lhs of
    LVar x      -> pretty x
    LSelect l s -> pretty l <> pretty s

  prettyList lhss = case lhss of
    [one] -> pretty one
    _     -> PP.parens (PP.hsep (map pretty lhss))
