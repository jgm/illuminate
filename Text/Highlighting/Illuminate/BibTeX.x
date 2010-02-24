{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.BibTeX where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$symbol = [\~ \# \$ \% \^ \& \_ \] \[]

@string = \" ([^ \" \\] | \\ .)* \" 
        | "``" ([^ \' \\] | \' [^ \'] | \\ .)* "''"
        | "`" ([^ \' \\] | \\ .)* "'"
 
tokens :-

<bracketed> {
  \}         { tok CBracket ==> popContext }
  \{         { tok CBracket ==> pushContext (bracketed, Plain) }
  \\ $alpha $alphanum* { tok Keyword }
  \\ [\' \` \^ \" \~ \= \.]   { tok Keyword }
}

<bibitem> {
  \{                  { tok CBracket }
  [^ \{ \, $white]*   { tok Label }
  \,                  { tok Symbol ==> pushContext (properties, Plain) }
}

<properties> {
  \}         { tok CBracket ==> popContext ==> popContext }
  \{         { tok CBracket ==> pushContext (bracketed, Plain) }
  $alpha [$alphanum \-]* { tok Keyword }
  \=         { tok Symbol }
  \,         { tok Symbol }
  @string    { tok String }
}

<0> {
  \% .*      { tok Comment }
  \@ $alpha+ { tok Type ==> pushContext (bibitem, Plain) }
}

 $white+     { tok Whitespace }
 .           { plain }

{
lexer :: Lexer
lexer = Lexer { name = "BibTeX"
              , aliases = ["bibtex"]
              , filenames = ["*.bib"]
              , scan = scanner }
}
