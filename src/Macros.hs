{-# LANGUAGE TemplateHaskell #-}

module Macros where

import Language.Haskell.TH

forceJust (Just x) = x
forceJust _ = undefined

expr = do
  exp_type <- forceJust <$> lookupTypeName "Exp"
  a <- forceJust <$> lookupTypeName "a"
  pure $ AppT (ConT exp_type) (VarT a)

var = do
  var_t <- forceJust <$> lookupTypeName "Var"
  a <- forceJust <$> lookupTypeName "a"
  pure $ AppT (ConT var_t) (VarT a)
