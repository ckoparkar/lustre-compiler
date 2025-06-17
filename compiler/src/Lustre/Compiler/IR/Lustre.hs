module Lustre.Compiler.IR.Lustre where

import Language.Lustre.AST
import Language.Lustre.Name
import Lustre.Compiler.Monad ( Unique, MonadGen(..), newUniq )
import Data.Text ( pack )
import AlexTools ( startPos )
import Data.Map qualified as Map
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP
import Language.Lustre.Pretty ( showPP )

--------------------------------------------------------------------------------

class TypeOf a where
  typeOf :: NodeDecl -> a -> [CType]

instance TypeOf Expression where
  typeOf nd expr = case expr of
    ERange _ e -> typeOf nd e
    Var x      -> [(nodeEnv nd) Map.! (nameToIdent x)]
    Lit c      -> case c of
                    Int{}  -> [CType IntType BaseClock]
                    Real{} -> [CType RealType BaseClock]
                    Bool{} -> [CType BoolType BaseClock]
    Const _ ty -> [ty]
    -- e `When` clk ->
    -- Tuple{} ->
    -- Array{} ->
    -- Select{} ->
    -- Struct{} ->
    UpdateStruct (Just nm) _ _ -> [CType (NamedType nm) BaseClock]
    -- WithThenElse{} ->
    -- Merge{}
    Call _ _ _ mbTys -> case mbTys of
                          Nothing  -> []
                          Just tys -> tys
    _ -> error $ "typeOf: TODO " ++ show expr


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


nodeEnv :: NodeDecl -> Map.Map Ident CType
nodeEnv nd =
  let (ins, outs, locals) = (nodeBinders nd)
  in Map.fromList $ map (\(Binder x ty) -> (x,ty)) (ins ++ outs ++ locals)

freshName :: MonadGen Unique m => m Name
freshName =
  do ident <- freshIdent
     pure $ Unqual ident

freshIdent :: MonadGen Unique m => m Ident
freshIdent =
  do lbl <- freshLabel
     pure $ Ident lbl Nothing

freshLabel :: MonadGen Unique m => m Label
freshLabel =
  do i <- newUniq
     let lbl = pack  ("x_" ++ show i)
     pure $ Label lbl (SourceRange (startPos lbl) (startPos lbl))

nameToIdent :: Name -> Ident
nameToIdent = origNameToIdent . nameOrigName

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
  pretty i = pretty (identText i)

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

instance Pretty PrimNode where
  pretty x = pretty (showPP x)

instance Pretty IClock where
  pretty x = pretty (showPP x)

instance Pretty Type where
  pretty x = pretty (showPP x)

instance Pretty CType where
  pretty (CType ty clk) = PP.parens (pretty (showPP ty) PP.<> pretty ","  PP.<> pretty (showPP clk))

instance Pretty e => Pretty (LHS e) where
  pretty lhs = case lhs of
    LVar x      -> pretty x
    LSelect l s -> pretty l PP.<> pretty s

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
