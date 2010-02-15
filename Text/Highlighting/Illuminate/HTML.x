{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.HTML  where
import qualified Text.Highlighting.Illuminate.CSS as CSS
import qualified Data.Foldable as F
import Data.Sequence ((<|))
}

%wrapper "illuminate"

$wordchar = [0-9a-zA-Z\_]
$symbol = [\~ \! \% \^ \* \( \) \- \+ \= \[ \] \" \: \; \, \. \/ \? \& \< \> \|]
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
@styletag = \< $white* [Ss][Tt][Yy][Ll][Ee] [^ \>]* \>
@endstyletag = \< $white* \/ $white* [Ss][Tt][Yy][Ll][Ee] $white* \>

tokens :-

 @styletag    { processStyle } 
 @endstyletag { tok Type }

 .           { plain }
 \n          { tok Whitespace }


{
processStyle (_,_,inp) len = Alex $ \s ->
  case CSS.scanner (alex_inp s) of
    Left e -> Left (show e)
    Right r -> Right (s', r')
      where s' = s{alex_pos = newpos, alex_inp = newinp, alex_chr = lastchar}
            r' = (Type, take len inp) <| r
            parsed_chars = F.concatMap snd r
            newpos = foldl alexMove (alex_pos s) parsed_chars
            lastchar = last parsed_chars
            newinp = drop (length parsed_chars) (alex_inp s)
}
