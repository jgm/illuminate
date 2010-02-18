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
<declaration> {
 [^ \>]+      { tok Preproc }
 \>           { tok Preproc ==> popContext }
}
<cdata> {
 [^ \]]+      { tok Preproc }
 "]]>"        { tok Preproc ==> popContext }
 \]           { tok Preproc }
}
<tag> {
 \>           { tok Tag ==> popContext }
 $wordchar+ / \=    { tok Keyword }
 \=            { tok Symbol }
 \" [^\"]* \"  { tok String }
 \' [^\']* \'  { tok String }
 [0-9]+        { tok Number }
 -- TODO: add lexer for attributes
}
<0> {
 [^ \< \&]+   { plain }
 @entity      { tok Entity }
 "<!--"       { tok Comment ==> pushContext (comment,Comment) }
 "<![CDATA["  { tok Preproc ==> pushContext (cdata,Plain) }
 \< [\! \?]   { tok Preproc ==> pushContext (declaration,Plain) }

 -- TODO: script tag
 @styletag    { tok Type ==> scanWith CSS.lexer  } 
 @endstyletag { tok Type }

 \< $white* [a-zA-Z0-9:]+ $white*  { tok Tag ==> pushContext (tag,Plain) }
 \< $white* \/ $white* [a-zA-Z0-9:]+ $white* \>  { tok Tag }

}
 $white+     { tok Whitespace }
 .           { plain }

{
lexer :: Lexer
lexer = Lexer { name = "HTML"
              , aliases = ["html","xhtml"]
              , extensions = ["html","xhtml","htm"]
              , scan = scanner }
}

