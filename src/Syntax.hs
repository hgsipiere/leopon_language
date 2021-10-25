{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

type Symbol = String

data Var = SimpleVar Symbol | FieldVar Var Symbol| SubscriptVar Var Exp
  deriving (Show, Eq)

data Exp
  = VarExp Var
  | NilExp
  | IntExp Int
  | StringExp String
  | CallExp {func :: Symbol, args :: [Exp]}
  | OpExp {left :: Exp, oper :: Oper, right :: Exp}
  | RecordExp {typ :: Symbol, fields :: [(Symbol, Exp)]}
  | SeqExp [Exp]
  | AssignExp {var :: Var, exp :: Exp}
  | IfExp {test :: Exp, then' :: Exp, else' :: Maybe Exp}
  | WhileExp {test :: Exp, body :: Exp}
  | ForExp { varFor :: Symbol, escape :: Bool, lo :: Exp, hi :: Exp, body :: Exp}
  | BreakExp
  | LetExp {decs :: [Dec], body :: Exp}
  | ArrayExp {typ :: Symbol, size :: Exp, init :: Exp}
  deriving (Show, Eq)

data Dec
  = FunctionDec Fundec
  | VarDec {name :: Symbol, escape :: Bool, typ :: Maybe Symbol, init :: Exp}
  | TypeDec (Symbol, SyntaxTy)
  deriving (Show, Eq)

data SyntaxTy = NameTy Symbol | RecordTy [Field] | ArrayTy Symbol
  deriving (Show, Eq)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
  deriving (Show, Eq)

data Field = Field {name :: Symbol, escape :: Bool, typ :: Symbol}
  deriving (Show, Eq)

data Fundec = Fundec {name :: Symbol, params :: [Field], resultTyp :: Maybe Symbol, body :: Exp}
  deriving (Show, Eq)
