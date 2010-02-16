{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Alex  where
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
}

%wrapper "illuminate"

$wordchar = [0-9a-zA-Z\_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \|]
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
@stringchars = [^ \" \\]+ | \\ .

tokens :-

<haskell> {
 ([^\{ \}]+ | \{ [^ \{ \}]+ \})*  { tokenizeWith Haskell.scanner }
 \}                               { tok Symbol ==> popContext } 
}

<0> {

 ^ \% "wrapper"                           { tok Preproc }
 ":-"                                     { tok Symbol }
 [\[ \] \-]                               { tok Symbol }
 \\ $symbol                               { tok Symbol }
 \{                                       { tok Symbol ==> pushContext (haskell, Plain) }
 \" @stringchars+ \"                      { tok String }
 "--" .*                                  { tok Comment }
}
 .           { plain }
 \n          { tok Whitespace }

