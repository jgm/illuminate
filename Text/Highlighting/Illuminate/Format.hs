module Text.Highlighting.Illuminate.Format (asANSI, asHtmlCSS, defaultCSS) where
import Text.Highlighting.Illuminate.Types
import Language.Haskell.HsColour.ANSI (highlight, Highlight(..), Colour(..))
import Data.Sequence (empty, (<|))
import qualified Data.Foldable as F
import Text.XHtml

-- | Collapse adjacent tokens with the same type.
consolidate :: Tokens -> Tokens
consolidate = collapse . F.foldr go (empty, Nothing)
  where go (curtype, str) (accum, Nothing) = (accum, Just (curtype, [str]))
        go (curtype, str) (accum, Just (t,xs)) | curtype == t =
               (accum, Just (curtype, str:xs))
        go (curtype, str) (accum, Just (t,xs)) =
               (collapse (accum, Just (t,xs)), Just (curtype, [str])) 
        collapse (accum, Nothing) = accum
        collapse (accum, Just (t,xs)) = (t, concat xs) <| accum

colorHighlight :: Token  -> ([Highlight], String)
colorHighlight (t, s) = (hls, s)
  where hls = case t of
               Keyword   -> [Foreground Green, Underscore]
               Symbol    -> [Foreground Red]
               String    -> [Foreground Green]
               Char      -> [Foreground Red]
               Number    -> [Foreground Magenta]
               Type      -> [Foreground Blue]
               Label     -> [Foreground Red, Underscore]
               Preproc   -> [Foreground Blue, Underscore]
               Function  -> [Foreground Blue, Bold]
               VarId     -> []
               ConId     -> [Foreground Blue]
               CBracket  -> [Foreground Red]
               Comment   -> [Foreground Cyan]
               Selector  -> [Foreground Blue]
               Property  -> [Foreground Green, Underscore]
               Tag       -> [Foreground Blue]
               Entity    -> [Foreground Green]
               Alert     -> [Background Cyan]
               _         -> []

bwHighlight :: Token -> ([Highlight], String)
bwHighlight (t, s) = (hls, s)
  where hls = case t of
               Keyword   -> [Underscore]
               Symbol    -> []
               String    -> []
               Char      -> []
               Number    -> []
               Type      -> []
               Label     -> [Underscore]
               Preproc   -> [Underscore]
               Function  -> [Bold]
               VarId     -> []
               ConId     -> []
               CBracket  -> []
               Comment   -> []
               Selector  -> []
               Property  -> [Underscore]
               Tag       -> []
               Entity    -> []
               Alert     -> [Bold]
               _         -> []

asANSI :: Bool -> Tokens -> String
asANSI colored = F.concatMap go . consolidate
 where go = hilite . (if colored then colorHighlight else bwHighlight)
       hilite (hls, s) = highlight hls s

asHtmlCSS :: Tokens -> [Html]
asHtmlCSS = F.toList . fmap go . consolidate
  where go (Whitespace, s) = stringToHtml s
        go (Plain, s) = stringToHtml s
        go (x, s) = thespan ! [theclass $ show x] << s

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
 \pre.sourceCode span.String { color: red; }\n\
 \pre.sourceCode span.Char { color: red; }\n\
 \pre.sourceCode span.Regex { color: orange; }\n\
 \pre.sourceCode span.Comment { color: gray; font-style: italic; }\n\
 \pre.sourceCode span.Preproc { color: #00008b; font-weight: bold; }\n\
 \pre.sourceCode span.VarId { }\n\
 \pre.sourceCode span.ConId { color: #006400; }\n\
 \pre.sourceCode span.Symbol { color: #8b0000; }\n\
 \pre.sourceCode span.Function { color: black; font-weight: bold; }\n\
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
 \pre.sourceCode span.Tag { color: blue; }\n\
 \pre.sourceCode span.Entity { color: green; }\n\
 \pre.sourceCode span.Value { color: #006400; font-style: italic; }\n\
 \pre.sourceCode span.Atom { color: orange; }  /* other */\n\
 \pre.sourceCode span.Meta { font-style: italic; }\n\
 \pre.sourceCode span.Path { color: orange; }\n\
 \pre.sourceCode span.Label { color: teal; font-weight: bold; }\n\
 \pre.sourceCode span.Error { color: purple; }\n\
 \pre.sourceCode span.Warning { color: #006400; }\n"


