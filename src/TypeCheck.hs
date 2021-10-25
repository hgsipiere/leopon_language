{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Applicative
import Data.Bifunctor

import Data.Either

import qualified Data.Map.Strict as M

import Syntax
import Util

-- we may need to pull away at the types until we get past all type identifiers

data Ty = TInt | TString | TRecord [(Symbol,Ty)] | TArray Ty | TNil | TUnit | TName Symbol (Maybe Ty) deriving (Show, Eq)

data EnvEntry = VarEntry Ty | FunEntry {formals :: [Ty], result :: Ty}

data ExpTy = ExpTy () Ty

type VEnv = M.Map Symbol EnvEntry
type TEnv = M.Map Symbol Ty

isInt (ExpTy _ x) = x == TInt

getTy (ExpTy _ x) = x

transField :: TEnv -> Field -> Either String (Symbol,Ty)
transField tenv (Field name _ typIdent) = (name,) <$> transTy tenv (NameTy typIdent)

transVar :: VEnv -> TEnv -> Var -> Either String ExpTy
transVar venv tenv (SimpleVar varName) = case M.lookup varName venv of
  Just (VarEntry ty) -> Right $ ExpTy () ty
  Just (FunEntry _ _) -> Left $ varName ++ " was expected to be a variable but found as function."
  _ -> Left $ varName ++ " was never bound and given a type in this scope. "

transVar venv tenv (SubscriptVar arrVar arrIndex) = do
  indexIsInt <- isInt <$> transExp venv tenv arrIndex
  if not indexIsInt then (Left $ show arrIndex ++ " is not an integer so not array index ") else do
    (ExpTy _ arrVarTy) <- transVar venv tenv arrVar
    case arrVarTy of
      TArray ty -> pure (ExpTy () ty)
      ty -> Left $ "Trying to index non-array " ++ show arrVar ++ " with type " ++ show arrVarTy

transVar venv tenv (FieldVar recordVar fieldName) = do
  recordTy <- transVar venv tenv recordVar
  case getTy $ recordTy of
    TRecord recordFieldTypes -> case lookup fieldName recordFieldTypes of
      Just ty -> pure (ExpTy () ty)
      Nothing -> Left $ "No field of name " ++ fieldName ++ " in record " ++ show recordVar
    _ -> Left $ "Trying to get field of non-record " ++ show recordVar


transTy :: TEnv -> SyntaxTy -> Either String Ty
transTy tenv (NameTy nameOfType) = case M.lookup nameOfType tenv of
  Just ty -> Right ty
  Nothing -> Left $ "Undefined type identifier " ++ show nameOfType

transTy tenv (RecordTy fields) = TRecord <$> mapM (transField tenv) fields

transTy tenv (ArrayTy elementTypeName) = TArray <$> transTy tenv (NameTy elementTypeName)


transDec :: VEnv -> TEnv -> Dec -> Either String (VEnv, TEnv)
transDec venv tenv (VarDec name _ Nothing init) = do
  bodyType <- getTy <$> transExp venv tenv init
  pure (M.insert name (VarEntry bodyType) venv, tenv)

transDec venv tenv (VarDec name _ (Just syntaxTypAnnot) init) = do
  typAnnot <- transTy tenv (NameTy syntaxTypAnnot)
  bodyTyp <- getTy <$> transExp venv tenv init
  if typAnnot == bodyTyp then pure (M.insert name (VarEntry bodyTyp) venv, tenv) else
    Left "type annotations and variable body type are different"

transDec venv tenv (TypeDec (name, syntaxTy)) = do
  typ <- transTy tenv syntaxTy
  pure (venv, M.insert name typ tenv)

transDec venv tenv (FunctionDec (Fundec name params Nothing body)) = do
  fieldTypes <- mapM (transField tenv) params
  let withFuncVEnv = M.insert name (FunEntry (fmap snd fieldTypes) bodyType) venv
  -- left biased union favours function parameters over
  -- already existing named variables with same name
  let withFuncVarsVEnv = M.union (M.fromList $ second VarEntry <$> fieldTypes) withFuncVEnv
  bodyType <- getTy <$> transExp withFuncVarsVEnv tenv body
  -- do not declare the localised function parameters as any params fall out of scope
  pure (withFuncVEnv, tenv)
  
transExp :: VEnv -> TEnv -> Exp -> Either String ExpTy
transExp venv tenv (OpExp left PlusOp right) = do
  leftIsInt <- isInt <$> transExp venv tenv left
  rightIsInt <- isInt <$> transExp venv tenv right
  case leftIsInt && rightIsInt of
    True -> Right (ExpTy () TInt)
    False -> Left (show left ++ "\n is meant to be Int\n" ++ show right ++ "\n is meant to be Int\n")
