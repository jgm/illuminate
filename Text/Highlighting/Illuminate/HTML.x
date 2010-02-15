{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.HTML  where
import qualified Text.Highlighting.Illuminate.CSS as CSS
}

%wrapper "illuminate"

$wordchar = [0-9a-zA-Z\_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \|]
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
@styletag = \< $white* [Ss][Tt][Yy][Ll][Ee] [^ \>]* \>
@endstyletag = \< $white* \/ $white* [Ss][Tt][Yy][Ll][Ee] $white* \>

tokens :-

 @styletag    { tok Type ==> scanWith CSS.scanner } 
 @endstyletag { tok Type }

 .           { plain }
 \n          { tok Whitespace }

