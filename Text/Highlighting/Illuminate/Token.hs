{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Text.Highlighting.Illuminate.Token (Token, TokenType(..), asANSI, asHtmlCSS, defaultCSS) where
import Language.Haskell.HsColour.ANSI (highlight, Highlight(..), Colour(..))
import Data.List (groupBy)
import Text.XHtml

data TokenType =
    Whitespace
  | Keyword
  | Symbol
  | String
  | Char
  | Number
  | Type
  | Label
  | Preproc
  | Function
  | Definition
  | VarId
  | ConId
  | CBracket
  | Comment
  | Selector
  | Property
  | Alert
  | Plain
  | EOF
  deriving (Eq,Show)

type Token = (TokenType, String)

asANSI :: [Token] -> String
asANSI = concatMap go . consolidate 
 where
  go (Keyword, s) = highlight [Foreground Green, Underscore] s
  go (Symbol, s)  = highlight [Foreground Red] s
  go (String, s)  = highlight [Foreground Green] s
  go (Char, s)    = highlight [Foreground Red] s
  go (Number, s)  = highlight [Foreground Magenta] s
  go (Type, s)    = highlight [Foreground Blue] s
  go (Label, s)   = highlight [Foreground Red, Underscore] s
  go (Preproc, s) = highlight [Foreground Blue, Underscore] s
  go (Function, s) = highlight [Foreground Blue, Bold] s
  go (Definition, s) = highlight [Foreground Blue, Bold] s
  go (VarId, s)    = highlight [] s
  go (ConId, s)    = highlight [Foreground Blue] s
  go (CBracket, s) = highlight [Foreground Red] s
  go (Comment, s) = highlight [Foreground Cyan] s
  go (Selector, s) = highlight [Foreground Blue] s
  go (Property, s) = highlight [Foreground Green, Underscore] s
  go (Alert, s)   = highlight [Background Cyan] s
  go (_, s)       = s

asHtmlCSS :: [Token] -> [Html]
asHtmlCSS = map go . consolidate
  where go (Whitespace, s) = stringToHtml s
        go (Plain, s) = stringToHtml s
        go (x, s) = thespan ! [theclass $ show x] << s

consolidate :: [Token] -> [Token]
consolidate = map mash . groupBy sameToken
  where mash ((x,y):zs) = (x, concat $ y : map snd zs)
        mash [] = undefined
        sameToken (x,_) (y,_) = x == y

defaultCSS :: String
defaultCSS =
 "/* default stylesheet for highlighting-alex */\n\
 \table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode, pre.sourceCode\n\
 \   { margin: 0; padding: 0; border: 0; vertical-align: baseline; border: none; }\n\
 \td.lineNumbers { text-align: right; background-color: #EBEBEB; color: black;\n\
 \     padding-right: 5px; padding-left: 5px; } \n\
 \td.sourceCode { padding-left: 5px; }\n\
 \pre.sourceCode { } /* default background and foreground colors here */\n\
 \pre.sourceCode span.Keyword { font-weight: bold; color: blue; } \n\
 \pre.sourceCode span.Type { color: #006400; }\n\
 \pre.sourceCode span.Definition { color: #006400; font-weight: bold; }\n\
 \pre.sourceCode span.String { color: red; }\n\
 \pre.sourceCode span.Char { color: red; }\n\
 \pre.sourceCode span.Regexp { color: orange; }\n\
 \pre.sourceCode span.Comment { color: gray; font-style: italic; }\n\
 \pre.sourceCode span.Preproc { color: #00008b; font-weight: bold; }\n\
 \pre.sourceCode span.VarId { }\n\
 \pre.sourceCode span.ConId { color: #006400; }\n\
 \pre.sourceCode span.Symbol { color: #8b0000; }\n\
 \pre.sourceCode span.Function, pre.sourceCode span.Definition \n\
 \    { color: black; font-weight: bold; }\n\
 \pre.sourceCode span.Alert { background-color: cyan; }\n\
 \pre.sourceCode span.Classname { color: teal; }\n\
 \pre.sourceCode span.Linenum { font-weight: bold; }\n\
 \pre.sourceCode span.Url { font-weight: bold; color: blue; }\n\
 \pre.sourceCode span.Date { font-weight: bold; color: blue; }  /* changelogs */\n\
 \pre.sourceCode span.Time, pre.sourceCode span.File { font-weight: bold; color: #00008b; }\n\
 \pre.sourceCode span.Ip, pre.sourceCode span.Name { color: #006400; }\n\
 \pre.sourceCode span.Variable { color: #006400; }\n\
 \pre.sourceCode span.Selector { color: #006400; }\n\
 \pre.sourceCode span.Property { color: blue; }\n\
 \pre.sourceCode span.Italics { color: #006400; font-style: italic; } /* LaTeX */\n\
 \pre.sourceCode span.Bold { color: #006400; font-style: bold; }\n\
 \pre.sourceCode span.Underline { color: #006400; text-decoration: underline; }\n\
 \pre.sourceCode span.Argument { color: #006400; }\n\
 \pre.sourceCode span.Optionalargument { color: purple; }\n\
 \pre.sourceCode span.Math { color: orange; }\n\
 \pre.sourceCode span.Bibtex { color: blue; }\n\
 \pre.sourceCode span.Oldfile { color: orange; }  /* diffs */\n\
 \pre.sourceCode span.Newfile { color: #006400; }\n\
 \pre.sourceCode span.Difflines { color: blue; }\n\
 \pre.sourceCode span.Selector { color: purple; }  /* css */\n\
 \pre.sourceCode span.Property { color: blue; }\n\
 \pre.sourceCode span.Value { color: #006400; font-style: italic; }\n\
 \pre.sourceCode span.Atom { color: orange; }  /* other */\n\
 \pre.sourceCode span.Meta { font-style: italic; }\n\
 \pre.sourceCode span.Path { color: orange; }\n\
 \pre.sourceCode span.Label { color: teal; font-weight: bold; }\n\
 \pre.sourceCode span.Error { color: purple; }\n\
 \pre.sourceCode span.Warning { color: #006400; }\n"

