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
import Foreign.Marshal.Utils ( fromBool )
import Data.Text ( Text, pack, unpack )

import Lustre.Compiler.Monad ( PassM )
import Lustre.Compiler.IR.Obc
import Lustre.Compiler.IR.Lustre ( nameToIdent, nameText, identText )
import Lustre.Compiler.IR.Base hiding (Lit, Var)

--------------------------------------------------------------------------------

codegenM :: Program -> PassM String
codegenM = pure . codegen

codegen :: Program -> String
codegen (Program decls) = asLib $ fold (map go decls)
  where
    go d = case d of
      DeclareType{}   -> error $ "TODO: " ++ show d
      DeclareConst{}  -> error $ "TODO: " ++ show d
      DeclareNode cls -> cgClass cls

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
  \"

cgClass :: ClassDecl -> CParts
cgClass c@(ClassDecl name _mems _insts fns) =
  CParts [render (mkClassStruct c)] (map (render . (cgMethod name)) fns)

mkClassStruct :: ClassDecl -> C.Definition
mkClassStruct (ClassDecl name mems insts _fns) =
  [C.cedecl| struct $id:name {$sdecls:fields}; |]
  where
    structTyPtr nm = [C.cty| struct $id:nm *|]
    fields = map (\(Binder x (CType ty _)) -> [C.csdecl| $ty:(cgType ty) $id:x; |]) mems ++
             map (\(f,x) ->  [C.csdecl| $ty:(structTyPtr f) $id:x; |] ) insts

cgMethod :: Text -> Method -> C.Definition
cgMethod cls (Method name (NodeBinders ins outs _locals) body) =
  C.FuncDef [C.cfun| void $id:mname ($params:params) {$items:cbody} |] noLoc
  where
    mname   = methodName cls name
    params  = [[C.cparam| struct $id:cls* $id:self |]] ++
              map (\(Binder x (CType ty _)) -> [C.cparam| $ty:(cgType ty) $id:x |]) ins ++
              map (\(Binder x (CType ty _)) -> [C.cparam| $ty:(cgType ty)* $id:x |]) outs
    cbody   = map C.BlockStm ((cgStmt (map binderDefines outs)) body)

cgStmt :: [Ident] -> Stmt -> [C.Stm]
cgStmt outs = go
  where
    go stmt = case stmt of
      Skip     -> []
      Do s1 s2 -> go s1 ++ go s2
      Let lhs e ->
        case lhs of
          LVar x -> if x `elem` outs
                    then [[C.cstm| *$id:x = $exp:(cgExpr outs e); |]]
                    else [[C.cstm| $id:x = $exp:(cgExpr outs e); |]]
          LSelect{} -> error $ "cgStmt: TODO " ++ show lhs


      LetState lhs e ->
        case lhs of
          LVar x -> [[C.cstm| $id:self->$id:x = $exp:(cgExpr outs e); |]]
          LSelect{} -> error $ "cgStmt: TODO " ++ show lhs

      If cnd thn els ->
        [[C.cstm| if ($exp:(cgExpr outs cnd)) {
                    $stms:(go thn)
                  } else {
                    $stms:(go els)
                  } |]]

      LetCall{..} ->
        let (cls,y) = lcClass
            mname   = methodName (nameText cls) lcRator
            args    = [[C.cexp| $id:self->$id:y |]] ++
                      map (cgExpr outs) lcRands ++
                      map (\lhs -> case lhs of
                             LVar x -> if x `elem` outs
                                       then [C.cexp| $id:x |]
                                       else [C.cexp| &$id:x |]
                             LSelect{} -> error $ "cgStmt: TODO " ++ show lhs)
                          lcBinds
        in [[C.cstm| $id:mname($args:args); |]]

cgExpr :: [Ident] -> Expr -> C.Exp
cgExpr outs = go
  where
    go expr = case expr of
      Lit c             -> [C.cexp| $const:c |]
      Var x
        | nameToIdent x `elem` outs
                        -> [C.cexp| *$id:x |]
        | otherwise     -> [C.cexp| $id:x |]
      SVar x            -> [C.cexp| $id:self->$id:x |]
      CallPrim pr ls    -> cgPrimApp pr (map go ls)

cgPrimApp :: PrimNode -> [C.Exp] -> C.Exp
cgPrimApp pr cargs = case (pr, cargs) of
  (Op2 Add, [e1, e2]) -> [C.cexp| $exp:e1 + $exp:e2 |]
  (Op2 Mul, [e1, e2]) -> [C.cexp| $exp:e1 * $exp:e2 |]
  oth -> error $ "cgPrimApp: Unexpected " ++ show oth

cgType :: Type -> C.Type
cgType ty = case ty of
  IntType  -> [C.cty| typename int64_t |]
  RealType -> [C.cty| double |]
  BoolType -> [C.cty| typename bool |]
  _         -> error $ "cgType: " ++ show ty

methodName :: Text -> Text -> Text
methodName cls name = cls <> (pack "_") <> name

render :: MPP.Pretty a => a -> String
render = MPP.pretty 80 . MPP.ppr

self :: String
self = "S"

--------------------------------------------------------------------------------

instance C.ToConst Literal where
  toConst c loc = case c of
    Int i  -> C.toConst i loc
    Real f -> C.toConst f loc
    Bool b -> C.toConst (fromBool @Int b) loc

instance C.ToIdent Name where
  toIdent x loc = C.toIdent (nameText x) loc

instance C.ToIdent Ident where
  toIdent x loc = C.toIdent (identText x) loc

instance C.ToIdent Text where
  toIdent x loc = C.toIdent (unpack x) loc
