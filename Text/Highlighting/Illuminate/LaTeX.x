{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.LaTeX where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$symbol = [\~ \# \$ \% \^ \_ \{ \} \\]
$hexdigit = [$digit A-F a-f]

@keyword = ("x"|"y")
@string = \" ([^ \" \\] | \\ .)* \" 

tokens :-

<0> {
  \% .*  { tok Comment }
}

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "LaTeX"
              , aliases = ["latex"]
              , filenames = ["*.tex","*.latex"]
              , scan = scanner }
}
