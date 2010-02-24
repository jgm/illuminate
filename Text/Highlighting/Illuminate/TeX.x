{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.TeX where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$symbol = [\~ \# \$ \% \^ \& \_ \\ \] \[]
$bracket = [\{ \}]
@string = \" ([^ \" \\] | \\ .)* \" 
        | "``" ([^ \' \\] | \' [^ \'] | \\ .)* "''"
        | "`" ([^ \' \\] | \\ .)* "'"
 
tokens :-

<beginend> {
 \{          { tok CBracket }
 $alpha $alphanum*  { tok Type }
 \}          { tok CBracket ==> popContext }
}

<0> {
  \% .*      { tok Comment }
  @string    { tok String }
  \\ ("begin"|"end")  { tok Keyword ==> pushContext (beginend, Type) }
  \\ $alpha $alphanum* { tok Keyword }
  \\ [\' \` \^ \" \~ \= \.]   { tok Keyword }
  $bracket          { tok CBracket }
  $symbol           { tok Symbol }
}

 $white+     { tok Whitespace }
 .           { plain }

{
lexer :: Lexer
lexer = Lexer { name = "TeX"
              , aliases = ["tex","latex","context"]
              , filenames = ["*.tex","*.latex","*.ctx"]
              , scan = scanner }
}
