{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.RHTML (lexer) where
import qualified Text.Highlighting.Illuminate.HTML as HTML
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
 ([^ \<] | \n | \< [^ \%])+  { tokenizeWith HTML.lexer }
}

{
lexer :: Lexer
lexer = Lexer { name = "RHTML"
              , aliases = ["rhtml"]
              , filenames = ["*.html.erb","*.rhtml"]
              , scan = scanner }
}

