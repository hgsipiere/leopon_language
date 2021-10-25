{

module Parser where

import Lexer
import Syntax2

}

%tokentype { Token }
%error {parseError}

%name parseExp exp
%name parseTy ty
%name parseDecs decs
%name parseExpSequence expSequence

%token
  while { While }
  for { For }
  to { To }
  break { Break }
  let { Let }
  in { In }
  end { End }
  function { Function }
  var { Var}
  type { Type }
  array { Array }
  if { If }
  then { Then }
  else { Else }
  do { Do }
  of { Of }
  nil { Nil }

  ':=' { Assign }
  '<' { LessThan }
  '>' { GreaterThan }
  '=' { Equals }
  '+' { Plus }
  ',' { Comma }
  ';' { SemiColon }
  ':' { Colon }
  '{' { LBrace }
  '}' { RBrace }
  '(' { LBracket }
  ')' { RBracket }
  '[' { LSquare }
  ']' { RSquare }
  '.' { Dot }
  ident { Ident $$ }
  strlit { StrLit $$ }
  intlit { IntLit $$ }
%%


tydec :: {(Symbol, Ty)}
tydec : type ident '=' ty { ($2,$4) }

ty :: {Ty}
   : ident { NameTy $1 }
   | '{' tyfields '}' {RecordTy $2}
   | array of ident {ArrayTy $3}

tyfields :: {[Field]}
         : ident ':' ident tyfieldsPrime { (Field $1 True $3) : $4 }
	 | {[]}
tyfieldsPrime :: {[Field]}
         : ',' ident ':' ident tyfieldsPrime { (Field $2 True $4) : $5 }
         | { [] }

lvalue :: {Var}
       : ident { SimpleVar $1 }
       | lvaluePrime { $1 }

lvaluePrime :: {Var}
            : ident '.' ident { FieldVar (SimpleVar $1) $3 }
            | lvaluePrime '.' ident { FieldVar $1 $3 }
	    | ident '[' exp ']' { SubscriptVar (SimpleVar $1) $3 }
            | lvaluePrime '[' exp ']' { SubscriptVar $1 $3 }

vardec :: {Dec}
       : var ident ':=' exp {VarDec $2 True Nothing $4}
       | var ident ':' ident exp {VarDec $2 True (Just $4) $5}

fundec :: {Fundec}
       : function ident '(' tyfields ')' '=' exp {Fundec $2 $4 Nothing $7}
       | function ident '(' tyfields ')' ':' ident '=' exp {Fundec $2 $4 (Just $7) $9}

decs :: {[Dec]}
     : tydec decs { (TypeDec $1) : $2 }
     | vardec decs { $1 : $2 }
     | fundec decs {FunctionDec $1 : $2 }
     | {[]}

exp :: {Exp}
exp : nil { NilExp }
    | break { BreakExp }
    | while exp do exp end { WhileExp $2 $4 }
    | if exp then exp end { IfExp $2 $4 Nothing }
    | if exp then exp else exp end { IfExp $2 $4 (Just $6) }
    | for ident ':=' exp to exp do exp end { ForExp $2 True $4 $6 $8 }
    | lvalue ':=' exp { AssignExp $1 $3 }
    | lvalue {VarExp $1}
    | ident '[' exp ']' of exp {ArrayExp $1 $3 $6}
    | strlit { StringExp $1}
    | ident '(' arguments ')' { CallExp $1 $3 }
    | ident '{' record '}' {RecordExp $1 $3 }
    | let decs in expSequence end { LetExp $2 (SeqExp $4) }
    | '(' expSequence ')' { SeqExp $2 }
    | intlit {IntExp $1}
    | exp '+' exp {OpExp $1 PlusOp $3}

arguments :: {[Exp]}
	 : exp argumentsPrime { $1 : $2 }
         | {[]}

argumentsPrime :: {[Exp]}
         : ',' exp argumentsPrime { $2 : $3 }
         | {[]}

record :: {[(Symbol, Exp)]}
      : ident '=' exp recordPrime { ($1, $3):$4 }
      | {[]}

recordPrime :: {[(Symbol,Exp)]}
	   : ',' ident '=' exp recordPrime { ($2,$4): $5 }
           | {[]}

expSequence :: {[Exp]}
            : exp expSequencePrime { $1 : $2 }
            | {[]}

expSequencePrime :: {[Exp]}
		 : ';' exp expSequencePrime { $2 : $3 }
		 | {[]}

{
parseError :: [Token] -> a
parseError (x:y:xs) = error ("Parsing this token got wrong " ++ show x ++ "with after" ++ show y)
parseError [x] = error ("Parsing with just this token left failed " ++ show x)
parseError _ = error "Parse error"
}
