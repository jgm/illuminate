{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.TeX (lexer) where
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

<label> {
 \{          { tok CBracket }
 \}          { tok CBracket ==> popContext }
}

<math> {
 \\ [\$ \\]  { tok Symbol }
 "$$"        { tok Math ==> popContext }
 \$          { tok Math ==> popContext }
 "\]"        { tok Math ==> popContext }
}

<0, math1> {
  \% .*      { tok Comment }
  @string    { tok String }
  \\ ("begin"|"end")  { tok Keyword ==> pushContext (label, Type) }
  \\ ("ref"|"label")  { tok Keyword ==> pushContext (label, Label) }
  \\ $alpha $alphanum* { tok Keyword }
  \\ [\' \` \^ \" \~ \= \.]   { tok Keyword }
  $bracket          { tok CBracket }
  "$$"              { tok Math ==> pushContext (math, Math) }
  \$                { tok Math ==> pushContext (math, Math) }
  "\["              { tok Math ==> pushContext (math, Math) }
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
