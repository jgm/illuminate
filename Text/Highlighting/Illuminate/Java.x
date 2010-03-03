{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Java (lexer) where
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
@keyword = ("abstract"|"assert"|"break"|"case"|"catch"|"class"|"const"|
           "continue"|"default"|"do"|"else"|"extends"|"false"|"final"|
           "finally"|"for"|"goto"|"if"|"implements"|"instanceof"|"interface"|
           "native"|"new"|"null"|"private"|"protected"|"public"|"return"|
           "static"|"strictfp"|"super"|"switch"|"synchronized"|"throw"|
           "throws"|"true"|"this"|"transient"|"try"|"volatile"|"while")

@type = ("int"|"byte"|"boolean"|"char"|"long"|"float"|"double"|"short"|"void")
@alert = (TODO|FIXME|BUG)[\:]?
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

<cl> {
  $white+       { tok Whitespace }
  $wordchar+    { tok Type ==> popContext } 
}

<package> {
  $white+            { tok Whitespace }
  [$wordchar \. \*]+ { tok ConId ==> popContext }
  \n                 { tok Whitespace ==> popContext }  
}

<0> {
  "/*"   { tok Comment ==> pushContext  (comment, Comment) }
  "//"   { tok Comment ==> pushContext  (linecomment, Comment) }
  ^ $white* $wordchar+ \:  { tok Label }
  ("class"|"interface") / $white  { tok Keyword ==> pushContext  (cl, Plain) } 
  ("import"|"package") / $white { tok Preproc ==> pushContext  (package, Plain) }
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
lexer = Lexer { name = "Java"
              , aliases = ["java"]
              , filenames = ["*.java"]
              , scan = scanner }
}

