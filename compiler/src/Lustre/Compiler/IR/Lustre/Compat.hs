{-# OPTIONS_GHC -Wno-orphans #-}

module Lustre.Compiler.IR.Lustre.Compat
  ( TypeOf(..), freshIdent, nodeBinders, bindLocals ) where

import Language.Lustre.AST
import Language.Lustre.Name as Name
import Lustre.Compiler.Monad ( Unique, MonadGen(..), newUniq )
import Data.Text ( pack )
import AlexTools ( startPos )
import Data.Map qualified as Map
import Data.List ( find )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP
import Language.Lustre.Pretty ( showPP )

import Lustre.Utils ( todo )

--------------------------------------------------------------------------------

class TypeOf a where
  typeOf :: [TypeDecl] -> NodeDecl -> a -> [CType]

instance TypeOf Expression where
  typeOf tyDecls nd expr = case expr of
    ERange _ e -> typeOf tyDecls nd e
    Var x      -> [lookupLocalName x]
    Lit c      -> case c of
                    Int{}  -> [CType IntType BaseClock]
                    Real{} -> [CType RealType BaseClock]
                    Bool{} -> [CType BoolType BaseClock]
    Const _ ty -> [ty]
    e `When` c -> let tys = typeOf tyDecls nd e
                  in map (\ty -> ty { cClock =  KnownClock c }) tys
    -- Tuple{} ->
    -- Array{} ->
    Select e s  ->
      case s of
        SelectField f -> case typeOf tyDecls nd e of
                           [CType (NamedType tyname) _] ->
                             let ftys = fieldTys tyname
                             in case find ((f ==) . fieldName) ftys of
                                  Nothing -> bad $ "Unknown field, " ++ show f
                                  Just ty -> [CType (fieldType ty) BaseClock]
                           oth -> bad $ "Not a named type, " ++ show oth
        oth -> todo oth
    Struct nm _ -> [CType (NamedType nm) BaseClock]
    UpdateStruct (Just nm) _ _ -> [CType (NamedType nm) BaseClock]
    -- WithThenElse{} ->
    Merge clk alts -> let clkclk = cClock (lookupLocalIdent clk)
                          MergeCase _ rhs = (head alts)
                          tys = typeOf tyDecls nd rhs
                      in map (\cty -> cty { cClock = clkclk } ) tys
    Call _ _ _ mbTys -> case mbTys of
                          Nothing  -> []
                          Just tys -> tys
    _ -> error $ "typeOf: TODO " ++ show expr
    where
      fieldTys nm =
        case find (\d -> (typeName d) == (nameToIdent nm)) tyDecls of
          Nothing   -> bad $ "Unknown type " ++ show nm
          Just decl -> case typeDef decl of
                         Nothing  -> bad $ "Abstract type " ++ show nm
                         Just def -> case def of
                                         IsStruct fs  -> fs
                                         IsType alias -> bad $ "Alias " ++ show alias
                                         IsEnum e     -> bad $ "Enum " ++ show e

      nameToIdent = Name.origNameToIdent . Name.nameOrigName

      nodeEnv nd0 =
        let (ins, outs, locals) = nodeBinders nd0
        in Map.fromList $ map (\(Binder x ty) -> (x,ty)) (ins ++ outs ++ locals)

      lookupLocalName x = (nodeEnv nd) Map.! (origNameToIdent (nameOrigName x))
      lookupLocalIdent x = (nodeEnv nd) Map.! x

bad :: String -> a
bad msg = error ("Unexpected " ++ msg)

instance Functor LHS where
  fmap f lhs = case lhs of
    LVar x -> LVar x
    LSelect lhs1 sel -> LSelect (fmap f lhs1) (fmap f sel)

instance Functor Selector where
  fmap f sel = case sel of
    SelectField x   -> SelectField x
    SelectElement e -> SelectElement (f e)
    SelectSlice e   -> SelectSlice (fmap f e)

instance Functor ArraySlice where
  fmap f (ArraySlice start end step) =
    ArraySlice (f start) (f end) (fmap f step)

--------------------------------------------------------------------------------

freshIdent :: MonadGen Unique m => m Ident
freshIdent =
  do i <- newUniq
     let txt    = pack "_x"
         lbl    = Label txt (SourceRange (startPos txt) (startPos txt))
         unqual = Ident lbl Nothing
         orig   = OrigName (fromInteger i) Nothing unqual AVal
     pure (Ident lbl (Just orig))

nodeBinders :: NodeDecl -> ([Binder], [Binder], [Binder])
nodeBinders nd =
  let (NodeProfile ins outs) = (nodeProfile nd)
      ins1 = foldr (\i acc -> case i of
                          InputBinder bndr -> bndr : acc
                          _ -> acc)
                   []
                   ins
      locals = case nodeDef nd of
                 Nothing -> []
                 Just (NodeBody lcls _) ->
                   foldr (\i acc -> case i of
                             LocalVar bndr -> bndr : acc
                             _ -> acc)
                         []
                         lcls
  in (ins1, outs, locals)

bindLocals :: [LocalDecl] -> NodeDecl -> NodeDecl
bindLocals new nd =
  case nodeDef nd of
    Nothing -> nd
    Just (NodeBody locals eqns) ->
      nd { nodeDef = Just (NodeBody (locals ++ new) eqns) }

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty Label where
  pretty = pretty . labText

instance Pretty Ident where
  pretty i = pretty (Name.identText i)

instance Pretty Name where
  pretty x = pretty (showPP x)

instance Pretty Literal where
  pretty x = pretty (showPP x)

instance Pretty TypeDecl where
  pretty x = pretty (showPP x)

instance Pretty ConstDef where
  pretty x = pretty (showPP x)

instance Pretty Binder where
  pretty x = pretty (showPP x)

instance Pretty TopDecl where
  pretty x = pretty (showPP x)
  prettyList xs = PP.vsep (PP.punctuate PP.line (map pretty xs))

instance Pretty PrimNode where
  pretty x = pretty (showPP x)

instance Pretty IClock where
  pretty x = pretty (showPP x)

instance Pretty Type where
  pretty x = pretty (showPP x)

instance Pretty ModName where
  pretty x = pretty (showPP x)

instance Pretty Thing where
  pretty x = pretty (showPP x)

instance Pretty CType where
  pretty (CType ty clk) = PP.parens (pretty (showPP ty) PP.<> pretty ","  PP.<> pretty (showPP clk))

instance Pretty e => Pretty (LHS e) where
  pretty lhs = case lhs of
    LVar x      -> pretty x
    LSelect l s -> pretty l PP.<> pretty s

  prettyList lhss = case lhss of
    [one] -> pretty one
    _     -> PP.parens (PP.hsep (map pretty lhss))

instance Pretty e => Pretty (Selector e) where
  pretty sel = case sel of
    SelectField i       -> pretty "." PP.<> pretty i
    SelectElement e     -> PP.brackets (pretty e)
    SelectSlice e       -> PP.brackets (pretty e)

instance Pretty e => Pretty (ArraySlice e) where
  pretty as = pretty (arrayStart as) PP.<+> pretty ".." PP.<+> pretty (arrayEnd as) PP.<+> mbStep
    where mbStep = case arrayStep as of
                     Nothing -> mempty
                     Just e  -> pretty "step" PP.<+> pretty e

instance Pretty e => Pretty (Field e) where
  pretty (Field x e) = pretty x PP.<+> pretty "=" PP.<+> pretty e
