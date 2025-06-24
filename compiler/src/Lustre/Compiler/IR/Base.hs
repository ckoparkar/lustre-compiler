module Lustre.Compiler.IR.Base
  ( BaseProgram(..), BaseTopDecl(..), BaseNodeDecl(..), BaseEqnGroup(..)
  , TypeDecl(..), TypeDef(..), FieldType(..), Selector(..), ConstDef(..), NodeBinders(..)
  , Binder(..), LHS(..), Type(..), Field(..)
  , CompName, mkCompName, mkCompName', compNameToString, compNameToText
  , compNameFromIdent, compNameFromName, compNameFromOrigName
  , nodeEnv, allBinders
  , FreeVars(..)
  , module Language.Lustre.AST
  , module Language.Lustre.Name
  ) where

import Language.Lustre.Name qualified as Name
import Language.Lustre.Name ( ModName(..), Thing(..) )
import Language.Lustre.AST ( Literal(..), ArraySlice(..)
                           , PrimNode(..), Op1(..), Op2(..), OpN(..), Iter(..)
                           )
import Lustre.Compiler.IR.Lustre.Compat ()
import Lustre.Compiler.Monad ( Unique, PassM, newUniq )
import Data.Text ( Text, pack, unpack )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP
import Data.Map qualified as Map
import Data.Set qualified as Set

--------------------------------------------------------------------------------

data BaseProgram a = Program [BaseTopDecl a]
  deriving Show

instance Functor BaseProgram where
  fmap f (Program ls) = Program (map (fmap f) ls)

data BaseTopDecl a
  = DeclareType  !TypeDecl
  | DeclareConst !ConstDef
  | DeclareNode  a
    deriving Show

instance Functor BaseTopDecl where
  fmap f decl = case decl of
    DeclareType a  -> DeclareType a
    DeclareConst a -> DeclareConst a
    DeclareNode nd -> DeclareNode (f nd)

-- | Declare a named type.
data TypeDecl = TypeDecl
  { typeName :: !CompName
  , typeDef  :: !(Maybe TypeDef)
    -- ^ Types with no definitions are abstract.
    -- We do not have good support for abstract types at the moment.
  } deriving Show

-- | A definition for a named type.
data TypeDef = IsType !Type
               -- ^ A type alias.
             | IsEnum ![ (CompName, Integer) ]
               -- ^ An enumeration type.
             | IsStruct ![ FieldType ]
               -- ^ A record type.
              deriving Show

-- | The type of the field of a structure.
data FieldType = FieldType
  { fieldName     :: Text               -- ^ The name of the field.
  , fieldType     :: Type               -- ^ The field's type.
  , fieldDefault  :: Maybe Literal
    -- ^ Optional default constant value, used if the field is omitted.
  } deriving Show

data Selector e
  = SelectField Text
  | SelectElement e
  | SelectSlice (ArraySlice e)
  deriving (Show, Eq, Ord)

instance Functor Selector where
  fmap f sel = case sel of
    SelectField x   -> SelectField x
    SelectElement e -> SelectElement (f e)
    SelectSlice e   -> SelectSlice (fmap f e)

-- | Note: only one of the type or definition may be "Nothing".
data ConstDef = ConstDef
  { constName     :: CompName
  , constType     :: Maybe Type   -- ^ Optional type annotation.
  , constDef      :: Maybe Literal
    {- ^ Optional definition. If the definition is omitted, then the constant
         is abstract.  In that case, the type cannot be omitted.

         Note that at the moment we don't have good support for abstract
         constants. -}
  } deriving Show

data BaseNodeDecl eqn ty = NodeDecl
  { nodeName    :: CompName
    -- ^ Node name

  , nodeBinders :: NodeBinders ty
    -- ^ Variables bound in a node

  , nodeEqns    :: [eqn]
    -- ^ Groups of recursive equations
  }
  deriving Show

data NodeBinders ty = NodeBinders
  { nodeInputs  :: [Binder ty]
  , nodeOutputs :: [Binder ty]
  , nodeLocals  :: [Binder ty]
  }
  deriving Show

instance Functor NodeBinders where
  fmap f (NodeBinders ins outs locals) =
    NodeBinders (map (fmap f) ins) (map (fmap f) outs) (map (fmap f) locals)

instance Semigroup (NodeBinders a) where
  (NodeBinders a b c) <> (NodeBinders z y x) =
    NodeBinders (a <> z) (b <> y) (c <> x)

instance Monoid (NodeBinders a) where
  mempty = NodeBinders [] [] []

-- | One or more equations.
data BaseEqnGroup eqn
  = NonRec eqn    -- ^ A non-recursive equation
  | Rec [eqn]     -- ^ A group of recursive equations.
  deriving Show

-- | Introduces a local variable.
data Binder ty = Binder
  { binderDefines :: CompName
  , binderType    :: ty
  }
  deriving Show

instance Functor Binder where
  fmap f (Binder x ty) = Binder x (f ty)

data LHS e
  = LVar CompName
  | LSelect (LHS e) (Selector e)
  deriving (Show, Eq, Ord)

instance Functor LHS where
  fmap f lhs = case lhs of
    LVar x -> LVar x
    LSelect lhs1 sel -> LSelect (fmap f lhs1) (fmap f sel)

-- | The type of value or a constant.
data Type
  = IntType     -- ^ Type of integers.
  | RealType    -- ^ Type of real numbers.
  | BoolType    -- ^ Type of boolean values.

  | NamedType CompName
    -- ^ A named type.  See 'TypeDef'.

  | ArrayType Type Integer
    -- ^ An array type.  The 'e' is for the size of the array.

  | IntSubrange Integer Integer
    -- ^ An interval subset of the integers.  The 'e's are bounds.
    -- Their values are included in the interval.
  deriving Show

data Field e = Field { fName :: Text, fValue :: e }
               deriving Show

instance Functor Field where
  fmap f (Field l e) = Field l (f e)

data CompName = CompName
  { cUniq   :: Unique
  , cText   :: Text
  , cModule :: Maybe Name.ModName
  , cThing  :: Name.Thing
  }
  deriving (Show, Eq, Ord)

mkCompName :: Text -> Maybe Name.ModName -> Name.Thing -> PassM CompName
mkCompName txt mo thing =
  do i <- newUniq
     pure (CompName i txt mo thing)

mkCompName' :: Text -> Maybe Name.ModName -> Name.Thing -> CompName
mkCompName' txt mo thing = CompName 0 txt mo thing

compNameToString :: CompName -> String
compNameToString = unpack . compNameToText

compNameToText :: CompName -> Text
compNameToText name = cText name <> pack (show (cUniq name))

compNameFromIdent :: Name.Ident -> CompName
compNameFromIdent = compNameFromOrigName . Name.identOrigName

compNameFromName :: Name.Name -> CompName
compNameFromName = compNameFromOrigName . Name.nameOrigName

compNameFromOrigName :: Name.OrigName -> CompName
compNameFromOrigName (Name.OrigName uniq mo unqual thing) =
  CompName (fromIntegral uniq) (Name.identText unqual) mo thing

--------------------------------------------------------------------------------

nodeEnv :: BaseNodeDecl eqn ty -> Map.Map CompName ty
nodeEnv nd =
  Map.fromList $ map (\(Binder x ty) -> (x,ty)) (allBinders (nodeBinders nd))

allBinders :: NodeBinders ty -> [Binder ty]
allBinders (NodeBinders ins outs locals) = ins ++ outs ++ locals

--------------------------------------------------------------------------------

class FreeVars a where
  freeVars :: a -> Set.Set CompName

instance FreeVars e => FreeVars (ArraySlice e) where
  freeVars (ArraySlice start end step) =
    freeVars start <> freeVars end <> (case step of
                                         Nothing -> Set.empty
                                         Just s  -> freeVars s)

instance FreeVars e => FreeVars (Selector e) where
  freeVars sel = case sel of
    SelectField{}   -> Set.empty
    SelectElement e -> freeVars e
    SelectSlice s   -> freeVars s

instance FreeVars e => FreeVars (LHS e) where
  freeVars lhs = case lhs of
    LVar x        -> Set.singleton x
    LSelect x sel -> freeVars x <> freeVars sel

instance FreeVars e => FreeVars (Field e) where
  freeVars (Field _ e) = freeVars e

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

instance Pretty ConstDef where
  pretty def = pretty "const" PP.<+> pretty (constName def) PP.<+>
                  opt ":" (constType def) PP.<+>
                  opt "=" (constDef def)
    where
    opt x y = case y of
                Nothing -> mempty
                Just a  -> pretty x PP.<+> pretty a

instance Pretty TypeDecl where
  pretty t = pretty "type" PP.<+> pretty (typeName t) PP.<+> mbDef
    where mbDef = case typeDef t of
                    Nothing -> PP.semi
                    Just d  -> pretty "=" PP.<+> pretty d PP.<> PP.semi

instance Pretty TypeDef where
  pretty td =
    case td of
      IsType t    -> pretty t
      IsEnum is   -> pretty "enum" PP.<+>
                     PP.braces (PP.hsep (PP.punctuate PP.comma (map (\(k,i) -> pretty k PP.<+> pretty "=" PP.<+> pretty i) is)))
      IsStruct fs -> PP.braces (PP.hcat (PP.punctuate (PP.semi PP.<> PP.space) (map pretty fs)))

instance Pretty FieldType where
  pretty ft = pretty (fieldName ft) PP.<+> pretty (fieldType ft) PP.<> optVal
    where optVal = case fieldDefault ft of
                     Nothing -> mempty
                     Just e  -> PP.space PP.<> pretty "=" PP.<+> pretty e

instance Pretty e => Pretty (Selector e) where
  pretty sel =
    case sel of
      SelectField i       -> pretty "." PP.<> pretty i
      SelectElement e     -> PP.brackets (pretty e)
      SelectSlice e       -> PP.brackets (pretty e)

instance Pretty a => Pretty (NodeBinders a) where
  pretty (NodeBinders ins outs locals) =
    PP.vsep [ prettyList ins
            , pretty "returns" PP.<+> prettyList outs PP.<> PP.semi
            , pretty "var " PP.<> PP.hsep (PP.punctuate PP.comma (map pretty locals)) PP.<> PP.semi
            ]

instance (Pretty e, Pretty ty) => Pretty (BaseNodeDecl e ty) where
  pretty nd = PP.vsep [ pretty "node" PP.<+> pretty (nodeName nd) PP.<> pretty (nodeBinders nd)
                      , pretty "let"
                      , PP.indent 4 (pretty (nodeEqns nd))
                      , pretty "tel"
                      ]

instance Pretty ty => Pretty (Binder ty) where
  pretty (Binder x ty) = pretty x PP.<> pretty ":" PP.<> pretty ty
  prettyList binds   = PP.tupled (map pretty binds)

instance Pretty e => Pretty (LHS e) where
  pretty lhs = case lhs of
    LVar x      -> pretty x
    LSelect l s -> pretty l <> pretty s

instance Pretty Type where
  pretty ty = case ty of
    NamedType x       -> pretty x
    ArrayType t e     -> pretty t PP.<+> pretty "^" PP.<+> pretty e
    IntType           -> pretty "int"
    RealType          -> pretty "real"
    BoolType          -> pretty "bool"
    IntSubrange e1 e2 ->
      pretty "subrange" PP.<+> PP.brackets (PP.hsep (PP.punctuate PP.comma (map pretty [e1,e2]))) PP.<+>
      pretty "of" PP.<+> pretty "int"

instance Pretty e => Pretty (Field e) where
  pretty (Field x e) = pretty x PP.<+> pretty "=" PP.<+> pretty e

instance Pretty CompName where
  pretty (CompName uniq txt mbmo _thing) =
    pretty txt PP.<> pretty "_" PP.<> pretty uniq
      PP.<> (case mbmo of
               Nothing -> mempty
               Just mo -> pretty ":" PP.<> pretty mo)
