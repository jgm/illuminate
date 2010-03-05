{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.D (lexer) where
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
@keyword = ("abstract"|"alias"|"align"|"asm"|"assert"|"auto"|"body"|
   "break"|"case"|"cast"|"catch"|"class"|"const"|"continue"|"debug"|"default"|
   "delegate"|"delete"|"deprecated"|"do"|"else"|"enum"|"export"|"extern"|
   "false"|"final"|"finally"|"for"|"foreach_reverse"|"foreach"|"function"|
   "goto"|"if"|"in"|"inout"|"interface"|"invariant"|"is"|"lazy"|
   "macro"|"mixin"|"module"|"new"|"nothrow"|"null"|"out"|"override"|
   "package"|"pragma"|"private"|"protected"|"public"|"pure"|
   "ref"|"return"|"scope"|"shared"|"static"|"struct"|"super"|"switch"|
   "synchronized"|"template"|"this"|"throw"|"true"|"try"|"typedef"|"typeid"|
   "typeof"|"union"|"unittest"|"version"|"void"|"volatile"|
   "while"|"with"|"__gshared"|"__thread"|"__traits")

@alert = (TODO|FIXME|BUG)[\:]?
@type = ("bool"|"char"|"double"|"float"|"int"|"long"|
         "short"|"signed"|"unsigned"|"void"|"wchar_t"|
         "byte"|"cdouble"|"cent"|"cfloat"|"creal"|"dchar"|
         "idouble"|"ifloat|ireal"|"real"|"ubyte"|"ucent"|
         "uint"|"ulong"|"ushort"|"wchar")
@string = \" ([^ \" \\] | \\ .)* \" [cwd]? |
          \` [^ \`]* \` |
          [rx] \" [^ \"]* \"
@char   = \' ([^ \' \\] | \\ .)* \'

tokens :-

<comment> {
  "*/"   { tok Comment ==> popContext }
  @alert { tok Alert }
}

<multilinecomment> {
  "+/"   { tok Comment ==> popContext }
  @alert { tok Alert }
}

<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}

<struct> {
  $white+       { tok Whitespace }
  $wordchar+    { tok Type ==> popContext } 
}

<include> {
  $white+       { tok Whitespace }
  \< [^ \>]* \> { tok String ==> popContext }
  \" @string \" { tok String ==> popContext }
}

<0> {
  "/*"   { tok Comment ==> pushContext  (comment, Comment) }
  "/+"   { tok Comment ==> pushContext  (multilinecomment, Comment) }
  "//"   { tok Comment ==> pushContext  (linecomment, Comment) }
  ^ "#!" { tok Comment ==> pushContext  (linecomment, Comment) }
  ^ $white* $wordchar+ \:  { tok Label }
  ("struct"|"class"|"typename") / $white  { tok Keyword ==> pushContext  (struct, Plain) } 
  ^ $white* \# $white* "include"  { tok Preproc ==> pushContext  (include, Plain) }
  ^ $white* \# $white* $wordchar*  { tok Preproc }
  [$white \n] ^ "__" $alpha+ "__" / [$white \n]  { tok Preproc }
  [$white \n] ^ "import" / $white  { tok Preproc }
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

{
lexer :: Lexer
lexer = Lexer { name = "D"
              , aliases = ["d"]
              , filenames = ["*.d"]
              , scan = scanner }
}

