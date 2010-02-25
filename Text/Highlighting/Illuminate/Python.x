{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Python  where
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
@keyword = ("and"|"assert"|"break"|"class"|"continue"|"def"|"del"|
            "elif"|"else"|"except"|"exec"|"finally"|"for"|"global"|
            "if"|"in"|"is"|"lambda"|"not"|"or"|"pass"|
           "print"|"raise"|"return"|"try"|"while")
@alert = (TODO|FIXME|BUG)[\:]?
@stringprefix = ([uU]|[rR]|[uU][rR]|[rR][uU])
@string = @stringprefix?
          ( \" ([^ \" \\] | \\ .)* \" 
          | \' ([^ \' \\] | \\ .)* \')

tokens :-

<strsq>  {
  \'\'\'    { tok String ==> popContext }
  $white+   { tok Whitespace }
}
<strdq>  {
  \"\"\"    { tok String ==> popContext }
  $white+   { tok Whitespace }
}
<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}
<def> {
  [$alpha \_] $wordchar*   { tok Function ==> popContext }
}
<0> {
  \#   { tok Comment ==> pushContext (linecomment, Comment) }
  "import"|"from" / $white { tok Preproc }
  $white ^ "def" / $white  { tok Keyword ==> pushContext (def, Plain) }
  @keyword / ~$wordchar  { tok Keyword }
  @number                { tok Number }
  \` [^ \`] \`           { tok String }
  @stringprefix? \"\"\"  { tok String ==> pushContext (strdq, String) } 
  @stringprefix? \'\'\'  { tok String ==> pushContext (strsq, String) } 
  @string                { tok String }
  $symbol                { tok Symbol }
  [\{ \}]                { tok Symbol }
  [$alpha \_]$wordchar*  { tok VarId }
  $white+                { tok Whitespace }
}

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Python"
              , aliases = ["python","py"]
              , filenames = ["*.py", "*.pyw", "*.sc", "SConstruct", "SConscript"]
              , scan = scanner }
}
