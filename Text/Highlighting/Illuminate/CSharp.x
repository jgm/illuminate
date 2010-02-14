{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.CSharp  where
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
@keyword = ("abstract"|"event"|"new"|"struct"|
 "as"|"explicit"|"null"|"switch"|
 "base"|"extern"|"this"|
 "false"|"operator"|"throw"|
 "break"|"finally"|"out"|"true"|
 "fixed"|"override"|"try"|
 "case"|"params"|"typeof"|
 "catch"|"for"|"private"|
 "foreach"|"protected"|
 "checked"|"goto"|"public"|"unchecked"|
 "class"|"if"|"readonly"|"unsafe"|
 "const"|"implicit"|"ref"|
 "continue"|"in"|"return"|
 "virtual"|
 "default"|"interface"|"sealed"|"volatile"|
 "delegate"|"internal"|
 "do"|"is"|"sizeof"|"while"|
 "lock"|"stackalloc"|
 "else"|"static"|
 "enum"|"namespace"|
 "get"|"partial"|"set"|
 "value"|"where"|"yield")

@type = ("bool"|"byte"|"sbyte"|"char"|"decimal"|"double"|
 "float"|"int"|"uint"|"long"|"ulong"|"object"|
 "short"|"ushort"|"string"|"void")

@alert = (TODO|FIXME|BUG)[\:]?
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
  ("struct"|"class"|"typename") / $white  { pushContext (struct, Plain) ==> tok Keyword } 
  ^ $white* \# $white* "include"  { pushContext (include, Plain) ==> tok Preproc }
  ^ $white* \# $white* $wordchar*  { tok Preproc }
  "using"  / ~$wordchar  { tok Preproc }
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

