module Text.Highlighting.Illuminate.Format (Colorer, defaultColors, bwColors, asANSI, asHtmlCSS, cssFor) where
import Text.Highlighting.Illuminate.Types
import Language.Haskell.HsColour.ANSI (highlight, Highlight(..), Colour(..))
import Data.Sequence (empty, (<|))
import qualified Data.Foldable as F
import Text.XHtml
import Data.Char (toLower)

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

type Colorer = TokenType -> [Highlight]

defaultColors :: Colorer
defaultColors  t = case t of
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

bwColors :: Colorer
bwColors t = case t of
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
                Comment   -> [Italic]
                Selector  -> []
                Property  -> [Underscore]
                Tag       -> []
                Entity    -> []
                Alert     -> [Bold]
                _         -> []

asANSI :: Colorer -> Tokens -> String
asANSI colors = F.concatMap go . consolidate
 where go = hilite . (\(t,s) -> (colors t, s))
       hilite (hls, s) = highlight hls s

asHtmlCSS :: Tokens -> [Html]
asHtmlCSS = F.toList . fmap go . consolidate
  where go (Whitespace, s) = stringToHtml s
        go (Plain, s)      = stringToHtml s
        go (x, s)          = thespan ! [theclass $ show x] << s

highlightsToCSSProperty :: Highlight -> String
highlightsToCSSProperty h =
  case h of
    Normal            -> ""
    Bold              -> "font-weight: bold; "
    Dim               -> ""
    Underscore        -> "text-decoration: underline; "
    Blink             -> "text-decoration: blink; "
    ReverseVideo      -> ""
    Concealed         -> "visibility: hidden; "
    Foreground Cyan   -> "color: gray; "  -- cyan is too hard to read in HTML
    Foreground c      -> "color: " ++ map toLower (show c) ++ "; "
    Background c      -> "background-color: " ++ map toLower (show c) ++ "; "
    Italic            -> "font-style: italic; " 

cssFor :: Colorer -> String
cssFor colors =
 "\n.sourceCode { margin: 0; padding: 0; border: 0; vertical-align: baseline; border: none; }\n\
 \td.lineNumbers { margin: 0; padding: 0; border: 0; vertical-align: baseline; border: none; text-align: right; background-color: #EBEBEB; color: black; padding-right: 5px; padding-left: 5px; } \n\
 \td.sourceCode { padding-left: 5px; }\n" ++
 concatMap (\tokType -> "pre.sourceCode span." ++ show tokType ++ " { " ++ concatMap highlightsToCSSProperty (colors tokType) ++ "}\n") allTokTypes
    where allTokTypes = [ Whitespace
                        , Keyword
                        , Symbol
                        , String
                        , Char
                        , Number
                        , Regex
                        , Type
                        , Label
                        , Preproc
                        , Function
                        , VarId
                        , ConId
                        , CBracket
                        , Comment
                        , Selector
                        , Property
                        , Tag
                        , Entity
                        , Alert ]

