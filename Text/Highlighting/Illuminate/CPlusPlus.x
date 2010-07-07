{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.CPlusPlus (lexer) where
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
@ckeyword = ("__asm"|"__cdecl"|"__declspec"|"__export"|"__far16"|
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
@cppkeyword = ("class"|"const_cast"|"delete"|
               "dynamic_cast"|"explicit"|"false"|"friend"|
               "inline"|"mutable"|"namespace"|"new"|"operator"|"private"|
               "protected"|"public"|"reinterpret_cast"|"static_cast"|
               "template"|"this"|"throw"|"true"|
               "try"|"typeid"|"typename"|"using"|"virtual")
@keyword = @ckeyword | @cppkeyword

@alert = (TODO|FIXME|BUG)[\:]?
@type = ("bool"|"char"|"double"|"float"|"int"|"long"|
         "short"|"signed"|"unsigned"|"void"|"wchar_t")
@string = \" ([^ \" \\] | \\ .)* \" 
@char   = \' ([^ \' \\] | \\ .)* \'

tokens :-

<comment> {
  "*/"   { tok Comment ==> popContext }
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
  @string       { tok String ==> popContext }
}

<0> {
  "/*"   { tok Comment ==> pushContext  (comment, Comment) }
  "//"   { tok Comment ==> pushContext  (linecomment, Comment) }
  ^ $white* $wordchar+ \:  { tok Label }
  ("struct"|"class"|"typename") / $white  { tok Keyword ==> pushContext  (struct, Plain) } 
  ^ $white* \# $white* "include"  { tok Preproc ==> pushContext  (include, Plain) }
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

{
lexer :: Lexer
lexer = Lexer { name = "C++"
              , aliases = ["c++","cplusplus","cpp"]
              , filenames = ["*.cpp","*.cc"]
              , scan = scanner }
}

