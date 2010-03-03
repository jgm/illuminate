{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.HTML (lexer) where
import qualified Text.Highlighting.Illuminate.CSS as CSS
import qualified Text.Highlighting.Illuminate.Javascript as Javascript 
}

%wrapper "illuminate"

$wordchar = [0-9a-zA-Z\_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \| \/]
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
@style = [Ss][Tt][Yy][Ll][Ee]
@styletag = \< $white* @style
@endstyletag = \< $white* \/ $white* @style $white* \>
@script = [Ss][Cc][Rr][Ii][Pp][Tt] 
@scripttag = \< $white* @script
@endscripttag = \< $white* \/ $white* @script $white* \>
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
<styletag> {
 \>           { tok Tag ==> scanWith CSS.lexer }
 @endstyletag { tok Tag ==> popContext }
}
<scripttag> {
 \>            { tok Tag ==> scanWith Javascript.lexer }
 @endscripttag { tok Tag ==> popContext }
}
<tag> {
 \>           { tok Tag ==> popContext }
}
<tag,styletag,scripttag> {
 $wordchar+ / \=    { tok Keyword }
 \=            { tok Symbol }
 \" [^\"]* \"  { tok String }
 \' [^\']* \'  { tok String }
 [0-9]+        { tok Number }
}
<0> {
 [^ \< \&]+   { plain }
 @entity      { tok Entity }
 "<!--"       { tok Comment ==> pushContext (comment,Comment) }
 "<![CDATA["  { tok Preproc ==> pushContext (cdata,Plain) }
 \< [\! \?]   { tok Preproc ==> pushContext (declaration,Plain) }

 @styletag     { tok Tag ==> pushContext (styletag,Plain) }
 @scripttag    { tok Tag ==> pushContext (scripttag,Plain) }
 \< $white* [a-zA-Z0-9:]+  { tok Tag ==> pushContext (tag,Plain) }
 \< $white* \/ $white* [a-zA-Z0-9:]+ $white* \>  { tok Tag }

}
 $white+     { tok Whitespace }
 .           { plain }

{
lexer :: Lexer
lexer = Lexer { name = "HTML"
              , aliases = ["html","xhtml"]
              , filenames = ["*.html","*.xhtml","*.htm"]
              , scan = scanner }
}

