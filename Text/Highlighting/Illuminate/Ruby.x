{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Ruby  where
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
@keyword = ("alias"|"begin"|"BEGIN"|"break"|"case"|"defined"|"do"|"else"|
           "elsif"|"end"|"END"|"ensure"|"for"|"if"|"in"|"include"|"loop"|
           "next"|"raise"|"redo"|"rescue"|"retry"|"return"|"super"|"then"|
           "undef"|"unless"|"until"|"when"|"while"|"yield"|"false"|"nil"|
           "self"|"true"|"__FILE__"|"__LINE__"|"and"|"not"|"or"|"def"|"class"|
           "module"|"catch"|"fail"|"load"|"throw")
@alert = (TODO|FIXME|BUG)[\:]?
@string = ( \" ([^ \" \\] | \\ .)* \" 
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
<blockcomment> {
  @alert { tok Alert }
  ^ "=end" / [ \t\r\n]  { tok Comment ==> popContext }
  $white+               { tok Whitespace }
}
<def> {
  [$alpha \_] $wordchar*   { tok Function ==> popContext }
}
<0> {
  ^ "=begin" / [ \t\r\n] { tok Comment ==> pushContext (blockcomment, Comment) }
  "require" / $white     { tok Preproc }
  \/ ([^\n\/]+|\\\/)* \/ { tok Regex }
  "%r" ( \{ (\\ \} | \# \{ $alphanum+ \} | [^ \} ])* \} )
                         { tok Regex }
  (\$ \#? | \@ \@?)($wordchar+ | \' | \" | \/)  { tok Type }
  $white ^ "def" / $white  { tok Keyword ==> pushContext (def, Plain) }
  @keyword / ~$wordchar  { tok Keyword }
  -- ? and ! as symbols as part of a method name
  $alphanum+ [\?\!]?     { tok VarId }
  @number                { tok Number }
  @string                { tok String }
  \` [^ \`] \`           { tok String }
  -- note: #{var} is not a comment:
  \# / [^\{]  { tok Comment ==> pushContext (linecomment, Comment) }

  -- TODO: here docs ... need some way to store and access the end string
  --  "<<" \-? (@string | $wordchar+)  { tok String ==> pushContext 

  $symbol                { tok Symbol }
  [\{ \}]                { tok CBracket }
  [$alpha \_]$wordchar*  { tok VarId }
  $white+                { tok Whitespace }
}

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Ruby"
              , aliases = ["ruby","rb"]
              , filenames = ["*.rb", "*.gemspec", "Rakefile"]
              , scan = scanner }
}
