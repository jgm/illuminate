module Text.Highlighting.Illuminate.Types ( Token
                                          , Tokens
                                          , TokenType(..)
                                          , Scanner
                                          , Lexer(..)
                                          ) where
import System.FilePath.GlobPattern (GlobPattern)
import Data.Sequence (Seq)

data TokenType =
    Whitespace
  | Keyword
  | Symbol
  | String
  | Char
  | Number
  | Regex
  | Type
  | Label
  | Preproc
  | Function
  | VarId
  | ConId
  | CBracket
  | Comment
  | Selector
  | Property
  | Tag
  | Entity
  | Math
  | NewFile
  | OldFile
  | Alert
  | Plain
  | EOF
  deriving (Eq,Show)

type Token = (TokenType, String)

type Tokens = Seq Token

type Scanner = String -> Either String Tokens

data Lexer = Lexer { name       :: String
                   , aliases    :: [String]
                   , filenames  :: [GlobPattern]
                   , scan       :: Scanner }
