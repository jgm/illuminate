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
@string = \" ([^ \" \\] | \\ .)* \" 
        | \' ([^ \' \\] | \\ .)* \'

tokens :-

<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}

<0> {
  \#   { tok Comment ==> pushContext (linecomment, Comment) }
  "import"|"from"        { tok Preproc }
  @keyword / ~$wordchar  { tok Keyword }
  @number                { tok Number }

-- TODO: multiline comments/strings
-- comment delim '^([[:space:]]*\'{3})' '(\'{3})' multiline 
-- comment delim '^([[:space:]]*\"{3})' '(\"{3})' multiline 

-- comment = '^([[:space:]]*\'(?:[^\\\']|\\.)*\'[[:space:]]*|[[:space:]]*\"(?:[^\\\"]|\\.)*\"[[:space:]]*)$'

-- string delim '([[:space:]]*\'{3})' '(\'{3})' multiline 
-- string delim '([[:space:]]*\"{3})' '(\"{3})' multiline 



  @string                { tok String }
  $symbol                { tok Symbol }
  [\{ \}]                { tok Symbol }
  [$alpha \_] $wordchar* $white* / \( { tok Function }
  [$alpha \_]$wordchar*  { tok VarId }
}


 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Python"
              , aliases = ["python","py"]
              , extensions = ["py"]
              , scan = scanner }
}
