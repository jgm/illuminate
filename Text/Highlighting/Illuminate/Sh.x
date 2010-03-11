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
@number = [\+ \-]? (@hexnumber | (($digit* \.)?  $digit+ ([eE] [\+\-]? $digit+)?))
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

<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}
<0> {
  \#   { tok Comment ==> pushContext (linecomment, Comment) }
  "import" / $white      {  tok Preproc }
  @keyword / ~$wordchar  { tok Keyword }
  @number                { tok Number }
  \\ [\" \']             { tok Symbol }
  @string                { tok String }
  "function" $white+ @ident $white* ("()")?  { tok Function }
  @ident $white* "()"    { tok Function }
  @ident / \=            { tok Variable }
  \$\{ [^ $white]+ \}    { tok Variable }
  \$ @ident              { tok Variable }
  \$ [\* \? \@ \!]       { tok Variable } 
  $symbol                { tok Symbol }

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
