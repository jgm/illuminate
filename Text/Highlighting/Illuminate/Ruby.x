{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Ruby (lexer) where
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
@singlestring = \' ([^ \' \\] | \\ .)* \'
@string = ( \" ([^ \" \\] | \\ .)* \" 
          | @singlestring
          | \` [^ \`]* \`)
@identifier = [$alpha \_] $wordchar*

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
<backtickstring> {
 \# \{                   { tok CBracket ==> pushContext (interp, Plain) }
 \\ .                    { tok String } 
 \`                      { tok String ==> popContext }
}
<doublestring> {
 \# \{                   { tok CBracket ==> pushContext (interp, Plain) }
 \\ .                    { tok String } 
 \"                      { tok String ==> popContext }
}
<heredoc> {
  @string                { tok String ==> hereDoc ==> popContext }
  @identifier            { tok VarId ==> hereDoc ==> popContext } 
  \# / [^\{]  { tok Comment ==> pushContext (linecomment, Comment) }
  [ \t]+                 { tok Whitespace }
  -- if we hit end of line w/o an identifier, exit context
  \n                     { tok Whitespace ==> popContext }
}
<interp>   \}            { tok CBracket ==> popContext }
<0,interp> {
  ^ "=begin" / [ \t\r\n] { tok Comment ==> pushContext (blockcomment, Comment) }
  "require" / $white     { tok Preproc }
  \/ ([^\n\/]+|\\\/)* \/ { tok Regex }
  "%r" ( \{ (\\ \} | \# \{ $alphanum+ \} | [^ \} ])* \} )
                         { tok Regex }
  (\$ \#? | \@ \@?)($wordchar+ | \' | \" | \/)  { tok Type }
  $white ^ "def" / $white  { tok Keyword ==> pushContext (def, Plain) }
  @keyword / ~$wordchar  { tok Keyword }
  -- ? and ! as symbols as part of a method name
  @identifier  [\?\!]?   { tok VarId }
  @number                { tok Number }
  @singlestring          { tok String }
  \`                     { tok String ==> pushContext (backtickstring, String) }
  \"                     { tok String ==> pushContext (doublestring, String) }
  \#                     { tok Comment ==> pushContext (linecomment, Comment) }

  "<<" \-?               { tok Symbol ==> pushContext (heredoc, Plain) }
  $symbol                { tok Symbol }
  [\{ \}]                { tok CBracket }
  @identifier            { tok VarId }
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
