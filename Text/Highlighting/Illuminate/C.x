{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.C  where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$wordchar = [$alphanum \_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \|]
$hexdigit = [$digit A-F a-f]

@hexnumber = "0x" $hexdigit+
@number = [\+ \-]? (@hexnumber |
                    (($digit* \.)?  $digit+ ([eE] [\+\-]? $digit+)?)
                   ) u? (("int" ("8"|"16"|"32"|"64")) | L)?
@keyword = ("__asm"|"__cdecl"|"__declspec"|"__export"|"__far16"|
            "__fastcall"|"__fortran"|"__import"|
            "__pascal"|"__rtti"|"__stdcall"|"_asm"|"_cdecl"|
            "__except"|"_export"|"_far16"|"_fastcall"|
            "__finally"|"_fortran"|"_import"|"_pascal"|"_stdcall"|
            "__thread"|"__try"|"asm"|"auto"|
            "break"|"case"|"catch"|"cdecl"|"const"|"continue"|"default"|
            "do"|"else"|"enum"|"extern"|"for"|"goto"|
            "if"|"pascal"|
            "register"|"return"|"sizeof"|"static"|
            "struct"|"switch"|
            "typedef"|"union"|
            "volatile"|"while")
@alert = (TODO|FIXME|BUG)[\:]?
@type = ("bool|char|double|float|int|long"|"short|signed|unsigned|void|wchar_t")
@string = \" ([^ \" \\] | \\ .)* \" 
@char   = \' ([^ \' \\] | \\ .)* \'

tokens :-

<comment> {
  "*/"   { popContext ==> tok Comment }
  @alert { tok Alert }
}

<linecomment> {
  @alert { tok Alert }
  \n     { popContext ==> tok Whitespace } 
}

<struct> {
  $white+       { tok Whitespace }
  $wordchar+    { popContext ==> tok Type } 
}

<include> {
  $white+       { tok Whitespace }
  \< [^ \>]* \> { popContext ==> tok String }
  \" @string \" { popContext ==> tok String }
}

<0> {
  "/*"   { pushContext (comment, Comment) ==> tok Comment }
  "//"   { pushContext (linecomment, Comment) ==> tok Comment }
  ^ $white* $wordchar+ \:   { tok Label }
  "struct" / $white  { pushContext (struct, Plain) ==> tok Keyword } 
  ^ $white* \# $white* "include"  { pushContext (include, Plain) ==> tok Preproc }
  ^ $white* \# $white* $wordchar*  { tok Preproc }
  @keyword / ~$wordchar  { tok Keyword }
  @type / ~$wordchar     { tok Type }
  @number                { tok Number }
  @string                { tok String }
  @char                  { tok Char }
  $symbol                { tok Symbol }
  [\{ \}]                { tok CBracket }
  [$alpha \_] $wordchar* $white* / \( { tok Function }
  [$alpha \_]$wordchar*  { tok VarId }
}


 .           { plain }
 \n          { tok Whitespace }

