module Text.Highlighting.Illuminate ( tokenize, languages, asANSI, asHtmlCSS, defaultCSS ) where
import Data.Char (toLower)
import Text.Highlighting.Illuminate.Token
import qualified Text.Highlighting.Illuminate.C as C
import qualified Text.Highlighting.Illuminate.Cabal as Cabal
import qualified Text.Highlighting.Illuminate.CPlusPlus as CPlusPlus
import qualified Text.Highlighting.Illuminate.CSharp as CSharp
import qualified Text.Highlighting.Illuminate.CSS as CSS
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
import qualified Text.Highlighting.Illuminate.HTML as HTML
import qualified Text.Highlighting.Illuminate.Java as Java
import qualified Text.Highlighting.Illuminate.LiterateHaskell as LiterateHaskell

tokenize :: String -> String -> Either String Tokens
tokenize lang source =
  case scannerFor (map toLower lang) of
        Just scan -> scan source
        Nothing   -> Left $ "Unknown language `" ++ lang ++ "'"

scannerFor :: String -> Maybe (String -> Either String Tokens)
scannerFor lang =
  let table = concatMap (\(ls,_,sc) -> map (\x -> (x,sc)) ls) langTable
  in  lookup (map toLower lang) table

languages :: [String]
languages = map (\(_,s,_) -> s) langTable

langTable :: [([String], String, (String -> Either String Tokens))]
langTable =
  [ (["haskell","hs"],            "Haskell",  Haskell.scanner)
  , (["literatehaskell", "lhs"],  "Literate Haskell", LiterateHaskell.scanner)
  , (["html","xhtml","htm"],      "HTML",     HTML.scanner)
  , (["c"],                       "C",        C.scanner)
  , (["java"],                    "Java",     Java.scanner)
  , (["cpp","cplusplus","c++"],   "C++",      CPlusPlus.scanner)
  , (["csharp","c#","cs"],        "C#",       CSharp.scanner)
  , (["css"],                     "CSS",      CSS.scanner)
  , (["cabal"],                   "Cabal",    Cabal.scanner)
  ]
