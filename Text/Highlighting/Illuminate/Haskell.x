{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Haskell  where
}

-- Based on haskell.x example from the alex distribution,
-- (c) Simon Marlow 2003

%wrapper "illuminate"

$wordchar  = [A-Za-z0-9 \_]

$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = 
  as|case|class|data|default|deriving|do|else|hiding|if|
  import|in|infix|infixl|infixr|instance|let|module|newtype|
  of|qualified|then|type|where

@reservedop =
  ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
   | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
   | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
   | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $white+ \\
@string  = $graphic # [\"\\] | " " | [\t] | @gap | @escape

@alert = (TODO|FIXME|BUG)[\:]?

tokens :-

<comment> {
  "-}"        { tok Comment ==> popContext }
  "{-"        { tok Comment ==> pushContext  (comment, Comment) }
  @alert      { tok Alert }
}

<linecomment> {
  \n          { tok Whitespace ==> popContext }
  @alert      { tok Alert }
}

<include> {
  $white+       { tok Whitespace }
  \< [^ \>]* \> { tok String ==> popContext }
  \" @string* \" { tok String ==> popContext }
}

<0,comment> "{-"  { tok Comment ==> pushContext  (comment, Comment) }

<0> {
 $white+       { tok Whitespace }
 "--"\-* / [^$symbol]    { tok Comment ==> pushContext  (linecomment, Comment) }
 ^ $white* \# $white* "include"  { tok Preproc ==> pushContext  (include, Plain) }
 ^ $white* \# $white* $wordchar*  { tok Preproc }

 $special      { tok Symbol }

 @reservedid   { tok Keyword }

 @conid (\. @conid)*	\.? { tok ConId }
 ^ @varid   { tok Function }
 @varid			{ tok VarId }

 @reservedop	{ tok Symbol }
 @varsym			{ tok Symbol }
 @consym			{ tok Symbol }

 @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal    { tok Number }

 @decimal \. @decimal @exponent?
  | @decimal @exponent    { tok Number }

 \' ($graphic # [\'\\] | " " | @escape) \' { tok Char }

 \" @string* \"    { tok String }
}

.       { plain}
\n      { tok Whitespace }

