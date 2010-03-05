module Text.Highlighting.Illuminate
         ( tokenize
         , Lexer(..)
         , lexers
         , lexerByName
         , lexerByFilename
         , module Text.Highlighting.Illuminate.Format ) where
import Data.Char (toLower)
import Data.List (find)
import Data.Sequence (singleton)
import System.FilePath
import System.FilePath.GlobPattern
import Text.Highlighting.Illuminate.Types
import Text.Highlighting.Illuminate.Format
import qualified Text.Highlighting.Illuminate.Alex as Alex
import qualified Text.Highlighting.Illuminate.BibTeX as BibTeX
import qualified Text.Highlighting.Illuminate.C as C
import qualified Text.Highlighting.Illuminate.Cabal as Cabal
import qualified Text.Highlighting.Illuminate.CPlusPlus as CPlusPlus
import qualified Text.Highlighting.Illuminate.CSharp as CSharp
import qualified Text.Highlighting.Illuminate.CSS as CSS
import qualified Text.Highlighting.Illuminate.D as D
import qualified Text.Highlighting.Illuminate.Diff as Diff
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
import qualified Text.Highlighting.Illuminate.HTML as HTML
import qualified Text.Highlighting.Illuminate.Java as Java
import qualified Text.Highlighting.Illuminate.Javascript as Javascript
import qualified Text.Highlighting.Illuminate.LiterateHaskell as LiterateHaskell
import qualified Text.Highlighting.Illuminate.Python as Python
import qualified Text.Highlighting.Illuminate.RHTML as RHTML
import qualified Text.Highlighting.Illuminate.RXML as RXML
import qualified Text.Highlighting.Illuminate.Ruby as Ruby
import qualified Text.Highlighting.Illuminate.TeX as TeX
import qualified Text.Highlighting.Illuminate.XML as XML

-- | Tokenize a string, returning either an error or a sequence
-- of tokens.  If the first argument is @Just@ a lexer, use
-- the lexer to tokenize.  If @Nothing@, return a single @Plain@
-- token with the whole source.  'tokenize' is designed to be
-- used with 'lexerByName' or 'lexerByFilename': for example,
-- 
-- > tokenize (lexerByName "Haskell") input
--
tokenize :: Maybe Lexer -> String -> Either String Tokens
tokenize (Just lexer) source = scan lexer source
tokenize Nothing source = Right $ singleton $ (Plain, source)

-- | Matches a lexer by name or alias (case-insensitive).
lexerByName :: String -> Maybe Lexer
lexerByName s = find matchName lexers
  where matchName l = map toLower s `elem` (map toLower (name l) : aliases l)

-- | Matches a lexer by the filename of the source file.
lexerByFilename :: String -> Maybe Lexer
lexerByFilename s = find matchFilename lexers
  where matchFilename l = any (\glob -> takeFileName s ~~ glob) (filenames l)

lexers :: [Lexer]
lexers = [ Alex.lexer
         , BibTeX.lexer
         , C.lexer
         , Cabal.lexer
         , CPlusPlus.lexer
         , CSharp.lexer
         , CSS.lexer
         , D.lexer
         , Diff.lexer
         , Haskell.lexer
         , HTML.lexer
         , Java.lexer
         , Javascript.lexer
         , LiterateHaskell.lexer
         , Python.lexer
         , Ruby.lexer
         , RHTML.lexer
         , RXML.lexer
         , TeX.lexer
         , XML.lexer
         ]
