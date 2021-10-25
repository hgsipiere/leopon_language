module Progs where

localRedecExample = "function f(v: int) = \n\
\let var v := 6\n\
\  in print(v);\n\
\     let var v := 6 in print (v) end;\n\
\     print(v);\n\
\     let var v := 8 in print (v) end;\n\
\     print(v)\n\
\  end"
