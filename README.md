# tiger

This is a project to learn how compilers work.

Currently it doesn't do line numbers for errors because I would like to be parsing differently.
I am currently looking into a clean type level approach to augmenting with line numbers.
I am thinking of finding some way to automatically generate incorrectly typed syntax trees for testing.
This currently lacks any tests although Prog.hs includes useful programs.
I would like to change from a parser generator to parser combinators but using the Alex lexer instead of normal character tokens.

