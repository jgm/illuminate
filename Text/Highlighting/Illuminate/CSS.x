{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.CSS  where
}

%wrapper "illuminate"

$wordchar = [0-9a-zA-Z\_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \|]
$digit = [0-9]
$hexdigit = [0-9a-fA-F]

@selector = [\. \#] $wordchar+
@alert = (TODO|FIXME|BUG)[\:]?
@property = ("azimuth" | "background" | "background-attachment" |
       "background-color" | "background-image" | "background-position" |
       "background-repeat" | "border" | "border-collapse" | "border-color" |
       "border-spacing" | "border-style" | "border-top" | "border-right" |
       "border-bottom" | "border-left" | "border-top-color" |
       "border-right-color" | "border-bottom-color" | "border-left-color" |
       "border-top-style" | "border-right-style" | "border-bottom-style" |
       "border-left-style" | "border-top-width" | "border-right-width" |
       "border-bottom-width" | "border-left-width" | "border-width" |
       "bottom" | "caption-side" | "clear" | "clip" | "color" | "content" |
       "counter-increment" | "counter-reset" | "cue" | "cue-after" |
       "cue-before" | "cursor" | "direction" | "display" | "elevation" |
       "empty-cells" | "float" | "font-family" | "font-size" |
       "font-size-adjust" | "font-stretch" | "font-style" | "font-variant" |
       "font-weight" | "font" | "height" | "left" | "letter-spacing" |
       "line-height" | "list-style" | "list-style-image" | "list-style-position" |
       "list-style-type" | "margin" | "margin-top" | "margin-right" |
       "margin-bottom" | "margin-left" | "marker-offset" | "marks" | "max-height" |
       "max-width" | "min-height" | "min-width" | "orphans" | "outline" |
       "outline-color" | "outline-style" | "outline-width" | "overflow" |
       "padding" | "padding-top" | "padding-right" | "padding-bottom" |
       "padding-left" | "page" | "page-break-after" | "page-break-before" |
       "page-break-inside" | "pause" | "pause-after" | "pause-before" |
       "pitch" | "pitch-range" | "play-during" | "position" | "quotes" |
       "richness" | "right" | "size" | "speak" | "speak-header" | "speak-numeral" |
       "speak-punctuation" | "speech-rate" | "stress" | "table-layout" | "text-align" |
       "text-decoration" | "text-indent" | "text-shadow" | "text-transform" | "top" |
       "unicode-bidi" | "vertical-align" | "visibility" | "voice-family" | "volume" |
       "white-space" | "widows" | "width" | "word-spacing" | "z-index")

@endstyletag = \< $white* \/ $white* [Ss][Tt][Yy][Ll][Ee] $white* \>

tokens :-

<comment> {
  "*/"   { tok Comment ==> popContext }
  @alert { tok Alert }
}

<linecomment> {
  @alert { tok Alert }
  \n     { tok Whitespace ==> popContext } 
}

<cbracket> {
  \# $hexdigit+                 { tok Number }
  $digit+                       { tok Number }
  @property / $white* \:        { tok Property }
  $symbol                       { tok Symbol }
  \}                            { tok CBracket ==> popContext }
}

<0> @endstyletag  { tok EOF }
<0,cbracket> "/*" { tok Comment ==> pushContext (comment, Comment) }
<0,cbracket> "//" .*   { tok Comment ==> pushContext (linecomment, Comment) }
<0>  @selector  { tok Selector }
<0>  $wordchar+ / ~$wordchar  { tok String }
<0>  \{   { tok CBracket ==> pushContext (cbracket, Plain) }
<0>  $symbol    { tok Symbol }

 .           { plain }
 \n          { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "CSS"
              , aliases = ["css"]
              , extensions = ["css"]
              , scan = scanner }
}


