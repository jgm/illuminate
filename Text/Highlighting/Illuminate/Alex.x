{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Alex  where
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$wordchar = [0-9 $alpha \_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \| \{ \} \= \^ \\ \$ \@]
@stringchars = [^ \" \\]+ | \\ .

tokens :-

<haskell> {
 ([^\{ \}]+ | \{ [^ \{ \}]+ \})*  { tokenizeWith Haskell.lexer }
 \}                               { tok CBracket ==> popContext } 
}
<0> {
 \< [^>]+ \> ($white* \{)?                { split "(<[^>]+>)( *)({?)" [Function, Whitespace, CBracket] }
 ^ \% "wrapper"                           { tok Preproc }
 \\ .                                     { tok Symbol }
 \}                                       { tok CBracket } -- end of context
 \$ $wordchar+                            { tok ConId }
 \@ $wordchar+                            { tok ConId }
 ":-"                                     { tok Symbol }
 \" @stringchars+ \"                      { tok String }
 "--" .*                                  { tok Comment }
 $symbol                                  { tok Symbol }
}

 $white+                                  { tok Whitespace }
 [^ \\]                                   { plain }
 \\                                       { plain }
{
lexer :: Lexer
lexer = Lexer { name = "Alex"
              , aliases = ["alex"]
              , extensions = ["x"]
              , scan = scanner }
}
