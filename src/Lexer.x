{
module Lexer where
}

%wrapper "basic"

$digit = 0-9                        -- digits
$alpha = [a-zA-Z]                -- alphabetic characters

tokens :-

  $white+ ;
  "--".* ;

  while { const While }
  for { const For }
  to { const To }
  break { const Break }
  let { const Let }
  in { const In }
  end { const End }
  function { const Function }
  var { const Var }
  type { const Type }
  array { const Array }
  if { const If }
  then { const Then }
  else { const Else }
  do { const Do }
  of { const Of }
  nil { const Nil }

  := { const Assign }
  \<> { const Fish }
  \<= { const LessThanEqual }
  >= { const GreaterThanEqual }

  \, { const Comma }
  : { const Colon }
  \; { const SemiColon }
  \( { const LBracket }
  \) { const RBracket }
  \[ { const LSquare }
  \] { const RSquare }
  \{ { const LBrace }
  \} { const RBrace }

  \+ { const Plus }
  \- { const Minus }
  \* { const Mult }
  \/ { const Divide }
  = { const Equals }
  \< { const LessThan }
  > { const GreaterThan }
  & { const Ampersand}
  \| { const Bar}
  \. { const Dot }
  \" [$alpha $digit \ ]* \"  { StrLit }
  $alpha[$alpha $digit \_]* {Ident}
  $digit+ {IntLit . read}

  {
-- Each action has type :: String -> Token

-- The token type:
data Token =
  While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function
  | Var
  | Type
  | Array
  | If
  | Then
  | Else
  | Do
  | Of
  | Nil
  | Comma
  | Colon
  | SemiColon
  | LBracket
  | RBracket
  | LSquare
  | RSquare
  | LBrace
  | RBrace
  | Dot
  | Plus
  | Minus
  | Mult
  | Divide
  | Equals
  | Fish
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Ampersand
  | Bar
  | Assign
  | Ident String
  | IntLit Int
  | StrLit String
  deriving (Eq, Show, Ord)

main = do
  s <- getContents
  print (alexScanTokens s)
}
