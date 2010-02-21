module Text.Highlighting.Illuminate ( tokenize, languages, lexerByName, lexerByExtension, module Text.Highlighting.Illuminate.Format ) where
import Data.Char (toLower)
import Data.List (find)
import Data.Sequence (singleton)
import Text.Highlighting.Illuminate.Types
import Text.Highlighting.Illuminate.Format
import qualified Text.Highlighting.Illuminate.Alex as Alex
import qualified Text.Highlighting.Illuminate.C as C
import qualified Text.Highlighting.Illuminate.Cabal as Cabal
import qualified Text.Highlighting.Illuminate.CPlusPlus as CPlusPlus
import qualified Text.Highlighting.Illuminate.CSharp as CSharp
import qualified Text.Highlighting.Illuminate.CSS as CSS
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
import qualified Text.Highlighting.Illuminate.HTML as HTML
import qualified Text.Highlighting.Illuminate.Java as Java
import qualified Text.Highlighting.Illuminate.Javascript as Javascript
import qualified Text.Highlighting.Illuminate.LiterateHaskell as LiterateHaskell

tokenize :: Maybe Lexer -> String -> Either String Tokens
tokenize (Just lexer) source = scan lexer source
tokenize Nothing source = Right $ singleton $ (Plain, source)

languages :: [String]
languages = map name lexers

lexerByName :: String -> Maybe Lexer
lexerByName s = find matchName lexers
  where matchName l = map toLower s `elem` (map toLower (name l) : aliases l)

lexerByExtension :: String -> Maybe Lexer
lexerByExtension s = find (\l -> s `elem` extensions l) lexers

lexers :: [Lexer]
lexers = [ Haskell.lexer
         , LiterateHaskell.lexer
         , Alex.lexer
         , HTML.lexer
         , C.lexer
         , Java.lexer
         , Javascript.lexer
         , CPlusPlus.lexer
         , CSharp.lexer
         , CSS.lexer
         , Cabal.lexer
         ]
