{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lustre.Compiler.Passes.Codegen
  ( codegenM, CParts(..) ) where

import Language.C.Pretty ()
import Language.C.Syntax qualified as C
import Language.C.Quote.C qualified as C
import Control.Monad.Reader qualified as Rd
import Text.PrettyPrint.Mainland qualified as MPP
import Text.PrettyPrint.Mainland.Class qualified as MPP
import Data.Foldable ( fold )
import Data.Loc ( noLoc )
import Data.List ( intersperse )
import Data.Text ( Text, unpack )
import Foreign.Marshal.Utils ( fromBool )
import Prettyprinter ( Pretty(..) )
import Prettyprinter qualified as PP

import Lustre.Utils ( todo )
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Obc

--------------------------------------------------------------------------------

data CParts = CParts
  { cHeader :: [String]
  , cFns    :: [String]
  }
  deriving (Eq, Show)

instance Semigroup CParts where
  cp1 <> cp2 = CParts { cHeader = cHeader cp1 <> cHeader cp2
                      , cFns    = cFns cp1 <> cFns cp2
                      }

instance Monoid CParts where
  mempty = CParts [] []

instance Pretty CParts where
  pretty (CParts hdr fns) = PP.vsep (PP.punctuate PP.line (map pretty (hdr ++ fns)))

stdIncludes :: String
stdIncludes =
  "\n\
  \#include<stdbool.h> \n\
  \#include<stdint.h> \n\
  \#include<string.h> \n\
  \"

codegenM :: Program -> PassM CParts
codegenM (Program decls) =
  do (CParts hdr fns) <- fold <$> runM (mapM go decls) namedTypes
     let hdr1 = intersperse "\n\n" (stdIncludes : hdr)
         fns1 = intersperse "\n\n" ("\n\n" : fns)
     pure (CParts hdr1 fns1)
  where
    go d = case d of
      DeclareType ty  -> cgTypeDecl ty
      DeclareNode cls -> cgClassDecl cls
      DeclareConst{}  -> todo d

    namedTypes =
      (\(a,b) -> NamedTypes a b) $
      foldr (\decl acc@(structs, enums) -> case decl of
                DeclareType d -> case typeDef d of
                                   Nothing  -> acc
                                   Just def -> case def of
                                                 IsEnum{}   -> (structs, typeName d : enums)
                                                 IsStruct{} -> (typeName d : structs, enums)
                                                 _          -> acc
                _ -> acc)
            ([],[])
            decls

--------------------------------------------------------------------------------

data NamedTypes = NamedTypes
  { ntStructs :: [CompName]
  , ntEnums :: [CompName]
  }
  deriving Show

type M a = Rd.ReaderT NamedTypes PassM a

runM :: M a -> NamedTypes -> PassM a
runM = Rd.runReaderT

--------------------------------------------------------------------------------

cgTypeDecl :: TypeDecl -> M CParts
cgTypeDecl tydecl =
  do def <- mkTypeStruct tydecl
     pure (CParts [render def] [])

mkTypeStruct :: TypeDecl -> M C.Definition
mkTypeStruct (TypeDecl name mbDef) = case mbDef of
  Nothing  -> pure [C.cedecl| struct $id:name; |]
  Just def -> case def of
    IsStruct fields ->
      do let cgField (FieldType fname ftype _mbDefault) =
               do ty <- cgType ftype
                  pure [C.csdecl| $ty:ty $id:fname; |]
         sdecls <- mapM cgField fields
         pure [C.cedecl| struct $id:name {$sdecls:sdecls}; |]
    IsEnum ls ->
      do let cgVariant (c,i) = [C.cenum| $id:c = $exp:i |]
         pure [C.cedecl| enum $id:name { $enums:(map cgVariant ls) }; |]
    oth -> todo oth

cgClassDecl :: ClassDecl -> M CParts
cgClassDecl c@(ClassDecl name _mems _insts fns) =
  do struct <- mkClassStruct c
     fns1 <- mapM (cgMethod name) fns
     pure $ CParts [render struct] (map render fns1)

mkClassStruct :: ClassDecl -> M C.Definition
mkClassStruct (ClassDecl name mems insts _fns) =
  do mems1 <- mapM (\(Binder x ty) -> do ty1 <- cgType ty
                                         pure [C.csdecl| $ty:ty1 $id:x; |])
                   mems
     let fields = mems1 ++ map (\(f,x) -> [C.csdecl| $ty:(structTyPtr f) $id:x; |]) insts
     pure [C.cedecl| struct $id:name {$sdecls:fields}; |]
  where
    structTyPtr nm = [C.cty| struct $id:nm *|]


cgMethod :: CompName -> Method -> M C.Definition
cgMethod cls (Method name (NodeBinders ins outs locals) body) =
  do ins1 <- mapM (\(Binder x ty) -> do ty1 <- cgType ty
                                        pure [C.cparam| $ty:ty1 $id:x |])
                  ins
     outs1 <- mapM (\(Binder x ty) -> do ty1 <- cgType ty
                                         pure [C.cparam| $ty:ty1* $id:x |])
                   outs
     locals1 <- mapM (\(Binder x ty) -> do ty1 <- cgType ty
                                           pure $ C.BlockDecl [C.cdecl| $ty:ty1 $id:x; |])
                    locals
     let mname  = methodName cls name
         params = [[C.cparam| struct $id:cls* $id:self |]] ++ ins1 ++ outs1
         cbody  = locals1 ++ map C.BlockStm (cgStmt body)
     pure $ C.FuncDef [C.cfun| void $id:mname ($params:params) {$items:cbody} |] noLoc


cgStmt :: Stmt -> [C.Stm]
cgStmt = go
  where
    go stmt = case stmt of
      Skip     -> []
      Do s1 s2 -> go s1 ++ go s2

      If cnd thn els ->
        case els of
          Skip -> [[C.cstm| if ($exp:(cgExpr cnd)) { $stms:(go thn) } |]]
          _    -> [[C.cstm| if ($exp:(cgExpr cnd)) { $stms:(go thn) } else { $stms:(go els) } |]]


      Switch cnd alts ->
        let alts1 = map (\(c,stm) -> [C.cstm| case $exp:(cgAtom (Lit c)) : {$stms:(cgStmt stm) break;} |])
                        alts
            body = C.Block [ C.BlockStm a | a <- alts1 ] noLoc
        in [[C.cstm| switch ($exp:(cgExpr cnd)) $stm:body |]]

      UpdateFields lhs updates ->
        map (\(Field lbl val) ->
              [C.cstm| ($exp:(cgLHS lhs)).$id:lbl = $exp:(cgExpr val); |])
            updates

      CopyStruct to from _tyname ->
        [[C.cstm| $exp:(cgLHS to) = $exp:(cgVar from); |]]

      Let lhs expr ->
        [[C.cstm| $exp:(cgLHS lhs) = $exp:(cgExpr expr); |]]

      LetAllocStruct _to _tyname -> []

      LetCall{..} ->
        let (cls,y) = lcClass
            mname   = methodName cls lcRator
            binds   = map (\lhs -> case lhs of
                                     LVar v -> LVar (Addr v)
                                     oth    -> todo oth)
                          lcBinds
            args    = [[C.cexp| $id:self->$id:y |]] ++
                      map cgExpr lcRands ++
                      map cgLHS binds
        in [[C.cstm| $id:mname($args:args); |]]

cgLHS :: LHS Expr -> C.Exp
cgLHS lhs = case lhs of
  LVar v    -> cgVar v
  LSelect{} -> todo lhs

cgExpr :: Expr -> C.Exp
cgExpr = go
  where
    go expr = case expr of
      Atom atom         -> cgAtom atom
      CallPrim pr ls _  -> cgPrimApp pr (map cgExpr ls)
      Select e s        -> case s of
                             SelectField f -> [C.cexp| $exp:(cgAtom e).$id:f |]
                             _ -> todo s

cgAtom :: Atom -> C.Exp
cgAtom atom = case atom of
  Lit c -> case c of
             Bool b -> if b then [C.cexp| true |] else [C.cexp| false |]
             _oth   -> [C.cexp| $const:c |]
  Var v -> cgVar v

cgVar :: Var -> C.Exp
cgVar v = case v of
  State x -> [C.cexp| $id:self->$id:x |]
  Out x   -> [C.cexp| *$id:x |]
  Oth x   -> [C.cexp| $id:x |]
  Addr x  -> go x
  where
    go x = case x of
      State{} -> cgVar x
      Out y   -> [C.cexp| $id:y |]
      Oth y   -> [C.cexp| &$id:y |]
      Addr z  -> go z

cgPrimApp :: PrimNode -> [C.Exp] -> C.Exp
cgPrimApp pr cargs = case (pr, cargs) of
  (Op2 Add, [e1, e2]) -> [C.cexp| $exp:e1 + $exp:e2 |]
  (Op2 Mul, [e1, e2]) -> [C.cexp| $exp:e1 * $exp:e2 |]
  (Op2 Eq,  [e1, e2]) -> [C.cexp| $exp:e1 == $exp:e2 |]
  (Op1 Not, [e1])     -> [C.cexp| !$exp:e1 |]
  oth -> todo oth

cgType :: Type -> M C.Type
cgType ty = case ty of
  IntType      -> pure [C.cty| typename int64_t |]
  RealType     -> pure [C.cty| double |]
  BoolType     -> pure [C.cty| typename bool |]
  NamedType nm -> do namedTys <- Rd.ask
                     if nm `elem` (ntStructs namedTys)
                       then pure [C.cty| struct $id:nm |]
                       else pure [C.cty| enum $id:nm |]
  _            -> todo ty

methodName :: CompName -> CompName -> String
methodName cls mthd =
  compNameToString cls <> "_" <> compNameToString mthd

render :: MPP.Pretty a => a -> String
render = MPP.pretty 80 . MPP.ppr

self :: String
self = "_SELF"

--------------------------------------------------------------------------------

instance C.ToConst Literal where
  toConst c loc = case c of
    Int i  -> C.toConst i loc
    Real f -> C.toConst f loc
    Bool b -> C.toConst (fromBool @Int b) loc

instance C.ToIdent CompName where
  toIdent x loc = C.toIdent (compNameToString x) loc

instance C.ToIdent Text where
  toIdent x loc = C.toIdent (unpack x) loc
