{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Alex  where
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$wordchar = [0-9 $alpha \_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \| \{ \} \= \^ \\]
@stringchars = [^ \" \\]+ | \\ .

tokens :-

<haskell> {
 ([^\{ \}]+ | \{ [^ \{ \}]+ \})*  { tokenizeWith Haskell.lexer }
 \}                               { tok CBracket ==> popContext } 
}
<0,context> {
 \< [^>]+ \> ($white* \{)?                { split "(<[^>]+>)( *)({?)" [Function, Whitespace, CBracket] }
 ^ \% "wrapper"                           { tok Preproc }
 \{                                       { tok CBracket ==> pushContext (haskell, Plain) }
 \}                                       { tok CBracket } -- end of context
 \\ $symbol                               { tok Symbol }
 \$ $wordchar+                            { tok ConId }
 \@ $wordchar+                            { tok ConId }
 ":-"                                     { tok Symbol }
 \" @stringchars+ \"                      { tok String }
 "--" .*                                  { tok Comment }
 $symbol                                  { tok Symbol }
}
 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Alex"
              , aliases = ["alex"]
              , extensions = ["x"]
              , scan = scanner }
}
