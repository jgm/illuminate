{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Javascript  where
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
@keyword = ("abstract"|"break"|"case"|"catch"|"class"|"const"|
            "continue"|"debugger"|"default"|"delete"|"do"|"else"|
            "enum"|"export"|"extends"|"false"|"final"|"finally"|
            "for"|"function"|"goto"|"if"|"implements"|"in"|"instanceof"|
            "interface"|"native"|"new"|"null"|"private"|"protected"|
            "prototype"|"public"|"return"|"static"|"super"|"switch"|
            "synchronized"|"throw"|"throws"|"this"|"transient"|"true"|
            "try"|"typeof"|"var"|"volatile"|"while"|"with")
@alert = (TODO|FIXME|BUG)[\:]?
@string = \" ([^ \" \\] | \\ .)* \" 
@char   = \' ([^ \' \\] | \\ .)* \'
@regexp = \/ (\\ .|[^ \* \\ \/])(\\.|[^ \\ \/])* \/ [gim]*
@script = [Ss][Cc][Rr][Ii][Pp][Tt] 
@endscripttag = \< $white* \/ $white* @script $white* \>

tokens :-

<comment> {
  "*/"   { tok Comment ==> popContext }
  @alert { tok Alert }
}

<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}

<include> {
  $white+       { tok Whitespace }
  \< [^ \>]* \> { tok String ==> popContext }
  \" @string \" { tok String ==> popContext }
}

<0> @endscripttag  { tok EOF }
<0> {
  "/*"   { tok Comment ==> pushContext (comment, Comment) }
  "//"   { tok Comment ==> pushContext (linecomment, Comment) }
  @keyword / ~$wordchar  { tok Keyword }

  -- these two cases are for /s that do NOT start a regex:
  @number $white* \/     { split "(.*)([ \\t\\n\\r]*)(\\/)" [Number, Whitespace, Symbol] }
  [$alpha \_] $wordchar* $white* \/  { split "(.*)([ \\t\\n\\r]*)(\\/)" [VarId, Whitespace, Symbol] }

  @regexp                { tok Regex }
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
lexer = Lexer { name = "Javascript"
              , aliases = ["javascript","js"]
              , filenames = ["*.js"]
              , scan = scanner }
}
