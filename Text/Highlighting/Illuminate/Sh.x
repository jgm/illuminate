{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Sh (lexer) where
}

%wrapper "illuminate"

$alpha = [A-Za-z]
$digit = [0-9]
$alphanum = [$alpha $digit]
$wordchar = [$alphanum \_]
$symbol = [\~ \! \% \^ \* \( \) \+ \= \[ \] \\ \: \; \, \. \/ \? \& \< \> \| \%]
$hexdigit = [$digit A-F a-f]

@hexnumber = "0x" $hexdigit+
@number = [\+ \-]? (@hexnumber |
                    (($digit* \.)?  $digit+ ([eE] [\+\-]? $digit+)?)
                   ) u? (("int" ("8"|"16"|"32"|"64")) | L)?
@keyword =("alias"|"bg"|"bind"|"break"|"builtin"|"caller"|"case"|"command"|"compgen"|
          "complete"|"continue"|"declare"|"dirs"|"disown"|"do"|"done"|"elif"|"else"|"enable"|
          "esac"|"eval"|"exec"|"exit"|"export"|"false"|"fc"|"fg"|"fi"|"for"|"getopts"|"hash"|"help"|
          "history"|"if"|"in"|"jobs"|"let"|"local"|"logout"|"popd"|"printf"|"pushd"|"read"|
          "readonly"|"return"|"select"|"set"|"shift"|"shopt"|"source"|"suspend"|"test"|"then"|
          "times"|"trap"|"true"|"type"|"typeset"|"umask"|"unalias"|"unset"|"until"|"wait"|"while")
@alert = (TODO|FIXME|BUG)[\:]?
@string = ( \" ([^ \" \\] | \\ .)* \" 
          | \' ([^ \' \\] | \\ .)* \')
@ident = [$alpha \_] $wordchar*

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
  "import" / $white      {  tok Preproc }
  @keyword / ~$wordchar  { tok Keyword }
  @number                { tok Number }
  @string                { tok String }
  \\ [\" \']             { tok Symbol }
  $symbol                { tok Symbol }
  "function" $white+ @ident $white* ("()")?  { tok Function }
  @ident $white* "()"    { tok Function }
-- # it is considered a variable if there's a =, which is discarded anyway
  @ident / \=            { tok VarId }
  \$\{ [^ $white]+ \}    { tok VarId }
  \$ @ident              { tok VarId }
  \$ [^ $white]{1}       { tok VarId } 

  [$alpha \_]$wordchar*  { tok Plain }
  $white+                { tok Whitespace }
}

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Sh"
              , aliases = ["sh","bash"]
              , filenames = ["*.sh", "*.bash"]
              , scan = scanner }
}
