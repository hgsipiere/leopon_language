module Progs where

decExample_Function = "function f(v: int) = \n\
\let var v := 6\n\
\  in print(v);\n\
\     let var v := 6 in print (v) end;\n\
\     print(v);\n\
\     let var v := 8 in print (v) end;\n\
\     print(v)\n\
\  end"

expExample_Let = "let type a = int\n\
\  var a : a := 5\n\
\  var b : a := a\n\
\  in b + a\n\
\end"
