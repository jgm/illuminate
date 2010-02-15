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
@entity = \& [^ $white]+ \;

tokens :-

<comment> {
 [^ \-]+      { tok Comment }
 "-->"        { tok Comment ==> popContext }
 \-           { tok Comment }
}
<ppa> {
 [^ \?]+      { tok Preproc }
 "?>"         { tok Preproc ==> popContext }
 \?           { tok Preproc }
}
<ppb> {
 [^ \!]+      { tok Preproc }
 "!>"         { tok Preproc ==> popContext }
 \!           { tok Preproc }
}
<cdata> {
 [^ \]]+      { tok Preproc }
 "]]>"        { tok Preproc ==> popContext }
 \]           { tok Preproc }
}
<tag> {
 \>           { tok Tag ==> popContext }
 -- TODO: add lexer for attributes
}
<0> {
 [^ \< \&]+   { plain }
 @entity      { tok Entity }
 "<!--"       { tok Comment ==> pushContext (comment,Comment) }
 "<![CDATA["  { tok Preproc ==> pushContext (cdata,Plain) }
 "<!"         { tok Preproc ==> pushContext (ppb,Plain) }
 "<?"         { tok Preproc ==> pushContext (ppa,Plain) }

 -- TODO: script tag
 @styletag    { tok Type ==> scanWith CSS.scanner } 
 @endstyletag { tok Type }

 \< $white* [a-zA-Z0-9:]+ $white*  { tok Tag ==> pushContext (tag,Plain) }
 \< $white* \/ $white* [a-zA-Z0-9:]+ $white* \>  { tok Tag }

}
 .           { plain }
 \n          { tok Whitespace }

