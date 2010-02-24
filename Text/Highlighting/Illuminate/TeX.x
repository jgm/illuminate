{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.TeX where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$symbol = [\~ \# \$ \% \^ \& \_ \{ \} \\ \] \[]

@string = \" ([^ \" \\] | \\ .)* \" 
        | "``" ([^ \' \\] | \' [^ \'] | \\ .)* "''"
        | "`" ([^ \' \\] | \\ .)* "'"
 
@keyword = ("x"|"y")

tokens :-

<0,bracketed> {
  \% .*      { tok Comment }
  @string    { tok String }
  \@ $alpha $alphanum* { tok Type }
  \\ $alpha $alphanum* { tok Keyword }
  \\ ("begin"|"end") \{ $alpha $alphanum* \}
     { split "(\\\\begin|\\\\end)({)([A-Za-z0-9]*)(})" [Keyword, Symbol, Type, Symbol] }
  \\ .              { tok Keyword }
  $symbol           { tok Symbol }
}

 $white+     { tok Whitespace }
 .           { plain }

{
lexer :: Lexer
lexer = Lexer { name = "TeX (including LaTeX, ConTeXt, BibTeX)"
              , aliases = ["tex","latex","context","bibtex"]
              , filenames = ["*.tex","*.latex","*.ctx","*.bib"]
              , scan = scanner }
}
