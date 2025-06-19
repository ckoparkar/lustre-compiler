{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Lustre.Compiler.Passes.Codegen
  ( codegenM, codegen ) where

import Language.C.Pretty ()
import Language.C.Syntax qualified as C
import Language.C.Quote.C qualified as C
import Text.PrettyPrint.Mainland qualified as MPP
import Text.PrettyPrint.Mainland.Class qualified as MPP
import Data.Foldable ( fold )
import Data.Loc ( noLoc )
import Data.List ( intersperse )
import Data.Text ( Text, unpack )
import Foreign.Marshal.Utils ( fromBool )

import Lustre.Utils ( todo )
import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Obc

--------------------------------------------------------------------------------

codegenM :: Program -> PassM String
codegenM = pure . codegen

codegen :: Program -> String
codegen (Program decls) = asLib $ fold (map go decls)
  where
    go d = case d of
      DeclareType ty  -> cgTypeDecl ty
      DeclareConst{}  -> todo d
      DeclareNode cls -> cgClassDecl cls

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

asLib :: CParts -> String
asLib (CParts hd fns) = fold $ intersperse "\n\n" (stdIncludes : (hd ++ fns))

stdIncludes :: String
stdIncludes =
  "\n\
  \#include<stdbool.h> \n\
  \#include<stdint.h> \n\
  \#include<string.h> \n\
  \"

cgTypeDecl :: TypeDecl -> CParts
cgTypeDecl tydecl = CParts [render (mkTypeStruct tydecl)] []

mkTypeStruct :: TypeDecl -> C.Definition
mkTypeStruct (TypeDecl name mbDef) = case mbDef of
  Nothing  -> [C.cedecl| struct $id:name; |]
  Just def -> case def of
    IsStruct fields ->
      let cgField (FieldType fname ftype _mbDefault) =
            [C.csdecl| $ty:(cgType ftype) $id:fname; |]
      in [C.cedecl| struct $id:name {$sdecls:(map cgField fields)}; |]
    oth -> todo oth

cgClassDecl :: ClassDecl -> CParts
cgClassDecl c@(ClassDecl name _mems _insts fns) =
  CParts [render (mkClassStruct c)] (map (render . (cgMethod name)) fns)

mkClassStruct :: ClassDecl -> C.Definition
mkClassStruct (ClassDecl name mems insts _fns) =
  [C.cedecl| struct $id:name {$sdecls:fields}; |]
  where
    structTyPtr nm = [C.cty| struct $id:nm *|]
    fields = map (\(Binder x ty) -> [C.csdecl| $ty:(cgType ty) $id:x; |]) mems ++
             map (\(f,x) ->  [C.csdecl| $ty:(structTyPtr f) $id:x; |] ) insts

cgMethod :: CompName -> Method -> C.Definition
cgMethod cls (Method name (NodeBinders ins outs locals) body) =
  C.FuncDef [C.cfun| void $id:mname ($params:params) {$items:cbody} |] noLoc
  where
    mname   = methodName cls name
    params  = [[C.cparam| struct $id:cls* $id:self |]] ++
              map (\(Binder x ty) -> [C.cparam| $ty:(cgType ty) $id:x |]) ins ++
              map (\(Binder x ty) -> [C.cparam| $ty:(cgType ty)* $id:x |]) outs
    cbody   = map (\(Binder x ty) -> C.BlockDecl [C.cdecl| $ty:(cgType ty) $id:x; |] ) locals ++
              map C.BlockStm (cgStmt body)

cgStmt :: Stmt -> [C.Stm]
cgStmt = go
  where
    go stmt = case stmt of
      Skip     -> []
      Do s1 s2 -> go s1 ++ go s2

      If cnd thn els ->
        [[C.cstm| if ($exp:(cgExpr cnd)) { $stms:(go thn) } else { $stms:(go els) } |]]

      UpdateFields lhs updates ->
        map (\(Field lbl val) ->
              [C.cstm| ($exp:(cgLHS lhs)).$id:lbl = $exp:(cgAtom val); |])
            updates

      Let lhs expr ->
        [[C.cstm| $exp:(cgLHS lhs) = $exp:(cgExpr expr); |]]

      LetState lhs expr ->
        [[C.cstm| $exp:(cgLHS lhs) = $exp:(cgExpr expr); |]]

      LetCopyStruct to from tyname ->
        let cgty = [C.cty| struct $id:tyname |]
        in [[C.cstm| memcpy($exp:(cgLHS to), $exp:(cgVar from), sizeof($ty:cgty)); |]]

      LetAllocStruct _to _tyname -> []

      LetCall{..} ->
        let (cls,y) = lcClass
            mname   = methodName cls lcRator
            args    = [[C.cexp| $id:self->$id:y |]] ++
                      map cgExpr lcRands ++
                      map cgLHS lcBinds
        in [[C.cstm| $id:mname($args:args); |]]

cgLHS :: LHS Atom -> C.Exp
cgLHS lhs = case lhs of
  LVar v    -> cgVar v
  LSelect{} -> todo lhs

cgExpr :: Expr -> C.Exp
cgExpr = go
  where
    go expr = case expr of
      Atom atom         -> cgAtom atom
      CallPrim pr ls    -> cgPrimApp pr (map cgAtom ls)
      Select e s        -> case s of
                             SelectField f -> [C.cexp| $exp:(cgAtom e).$id:f |]
                             _ -> todo s

cgAtom :: Atom -> C.Exp
cgAtom atom = case atom of
  Lit c -> [C.cexp| $const:c |]
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
  oth -> todo oth

cgType :: Type -> C.Type
cgType ty = case ty of
  IntType      -> [C.cty| typename int64_t |]
  RealType     -> [C.cty| double |]
  BoolType     -> [C.cty| typename bool |]
  NamedType nm -> [C.cty| struct $id:nm |]
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
