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
<context> {
  \{                              { tok CBracket ==> popContext }
  $white* \n $white*              { tok Whitespace ==> popContext }
}
<0> {
 \< [^>]+ \>                              { tok Function ==> pushContext (context, Plain)}
 ^ \% "wrapper"                           { tok Preproc }
 \\ .                                     { tok Symbol }
 \}                                       { tok CBracket }  -- end context
 \{                                       { tok CBracket ==> pushContext (haskell, Plain) }
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
              , filenames = ["*.x"]
              , scan = scanner }
}
