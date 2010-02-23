{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Cabal  where
}

%wrapper "illuminate"

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
   GPL
  |LGPL
  |BSD3
  |BSD4
  |PublicDomain
  |AllRightsReserved
  |OtherLicense
  |if
  |[Ff]lag
  |else
  |[Ee]xecutable
  |[Ll]ibrary
  |[Oo][Ss]
  |[Aa]rch
  |[Tt]rue
  |[Ff]alse
  |[Ii]mpl

@regfieldid =
   [Aa]uthor
  |[Bb]ug\-[Rr]eports
  |[Bb]uild\-[Dd]epends
  |[Bb]uild\-[Tt]ype
  |[Bb]uild\-[Tt]ools
  |[Bb]uildable
  |[Cc]\-[Ss]ources
  |[Cc][Cc]\-[Oo]ptions
  |[Cc]abal\-[Vv]ersion
  |[Cc]ategory
  |[Cc]opyright
  |[Dd]ata\-[Dd]ir
  |[Dd]ata\-[Ff]iles
  |[Dd]efault
  |[Dd]escription
  |[Ee]xecutable
  |[Ee]xposed
  |[Ee]xposed\-[Mm]odules
  |[Ee]xtensions
  |[Ee]xtra\-[Ll]ibraries
  |[Ee]xtra\-[Ll]ib\-[Dd]irs
  |[Ee]xtra\-[Ss]ource\-[Ff]iles
  |[Ee]xtra\-[Tt]mp\-[Ff]iles
  |[Ff]rameworks
  |[Gg][Hh][Cc]\-[Oo]ptions
  |[Gg][Hh][Cc]\-[Pp]rof\-[Oo]ptions
  |[Gg][Hh][Cc]\-[Ss]hared\-[Oo]ptions
  |[Hh][Uu][Gg][Ss]\-[Oo]ptions
  |[Nn][Hh][Cc]98\-[Oo]ptions
  |[Hh]omepage
  |[Hh][Ss]\-[Ss]ource\-[Dd]irs
  |[Ii]nclude\-[Dd]irs
  |[Ii]ncludes
  |[Ii]nstall\-[Ii]ncludes
  |[Ll]icense
  |[Ll]icense\-[Ff]ile
  |[Ll][Dd]\-[Oo]ptions
  |[Mm]ain\-[Ii]s
  |[Mm]aintainer
  |[Nn]ame
  |[Oo]ther\-[Mm]odules
  |[Pp]ackage\-[Uu][Rr][Ll]
  |[Pp]kgconfig\-[Dd]epends
  |[Ss]tability
  |[Ss]ynopsis
  |[Tt]ested\-[Ww]ith
  |[Vv]ersion

@sourcerepofieldid =
   [Tt]ype
  |[Ll]ocation
  |[Mm]odule
  |[Bb]ranch
  |[Tt]ag
  |[Ss]ubdir

@fieldid = @regfieldid | @sourcerepofieldid

@reservedop =
        ">" | ">=" | "<" | "<="

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

main :-

<comment> {
 .*                                       { tok Comment }
 \n                                       { tok Whitespace ==> popContext }
}

<cond> {
 "if" | "flag" | "else"                   { tok Keyword }
 $white+ / "flag"                         { tok Whitespace }
 $white+                                  { tok Whitespace ==> popContext }
}

<field> {
 @reservedid                              { tok Keyword ==> popContext }
 [ \t]                                    { tok Whitespace }
 [. \n]                                   { plain ==> popContext }
}

<0> {
 ^ $white* / "--"                         { tok Whitespace ==> pushContext  (comment, Comment) } 
 @reservedop                              { tok Symbol }
 @fieldid ":"                             { tok Type ==> pushContext  (field, Plain) }
 ^ @reservedid                            { tok Keyword }
 ^ $white* / "if"|"else"                  { tok Whitespace ==> pushContext (cond, Plain) }
}

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Cabal"
              , aliases = ["cabal"]
              , filenames = ["*.cabal"]
              , scan = scanner }
}
