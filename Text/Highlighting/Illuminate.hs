module Text.Highlighting.Illuminate ( tokenize, languages, lexerByName,
  lexerByFilename, module Text.Highlighting.Illuminate.Format ) where
import Data.Char (toLower)
import Data.List (find)
import Data.Sequence (singleton)
import System.FilePath.GlobPattern
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
import qualified Text.Highlighting.Illuminate.TeX as TeX
import qualified Text.Highlighting.Illuminate.LiterateHaskell as LiterateHaskell
import qualified Text.Highlighting.Illuminate.Python as Python

tokenize :: Maybe Lexer -> String -> Either String Tokens
tokenize (Just lexer) source = scan lexer source
tokenize Nothing source = Right $ singleton $ (Plain, source)

languages :: [String]
languages = map name lexers

lexerByName :: String -> Maybe Lexer
lexerByName s = find matchName lexers
  where matchName l = map toLower s `elem` (map toLower (name l) : aliases l)

lexerByFilename :: String -> Maybe Lexer
lexerByFilename s = find matchFilename lexers
  where matchFilename l = any (\glob -> s ~~ glob) (filenames l)

lexers :: [Lexer]
lexers = [ Alex.lexer
         , C.lexer
         , Cabal.lexer
         , CPlusPlus.lexer
         , CSharp.lexer
         , CSS.lexer
         , Haskell.lexer
         , HTML.lexer
         , Java.lexer
         , Javascript.lexer
         , LiterateHaskell.lexer
         , TeX.lexer
         , Python.lexer
         ]
