{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.RXML (lexer) where
import qualified Text.Highlighting.Illuminate.XML as XML
import qualified Text.Highlighting.Illuminate.Ruby as Ruby
}

%wrapper "illuminate"

tokens :-

<erb> {
  ([^ \%] | \n | \% [^ \>])+  { tokenizeWith Ruby.lexer }
  \% \>                       { tok Tag ==> popContext }
}
<0> {
 \< \% \=?                   { tok Tag ==> pushContext (erb, Plain) }
 ([^ \<] | \n | \< [^ \%])+  { tokenizeWith XML.lexer }
}

{
lexer :: Lexer
lexer = Lexer { name = "RXML"
              , aliases = ["rxml"]
              , filenames = ["*.xml.erb","*.rxml"]
              , scan = scanner }
}

