module Text.Highlighting.Illuminate.Format (Options(..), defaultOptions,
         Style, Styling(..), Color(..),
         colorful, monochrome, toANSI, toLaTeX, toHtmlCSS, toHtmlCSSInline,
         cssFor, toHtmlInline) where
import Text.Highlighting.Illuminate.Types
import qualified Language.Haskell.HsColour.ANSI as ANSI
import Data.Sequence (empty, (<|))
import qualified Data.Foldable as F
import qualified Text.XHtml as XHtml
import qualified Text.Html as Html
import Data.Char (toLower)
import Data.Bits (shiftR, (.&.))
import Text.Printf (printf)

data Options = Options { optStyle       :: Style
                       , optNumberLines :: Bool
                       , optStartNumber :: Int
                       }

defaultOptions :: Options
defaultOptions = Options { optStyle = colorful
                         , optNumberLines = False
                         , optStartNumber = 1 }

-- | A Style is a generic instruction for formatting a token of the given
-- type.  The same style can be used for various output formats (HTML,
-- ANSI, LaTeX...).
type Style = TokenType -> [Styling]

-- | Colours supported by ANSI codes.
data Color = Aqua | Black | Blue | Fuchsia | Gray | Green | Lime | Maroon |
             Navy | Olive | Purple | Red | Silver | Teal | White | Yellow |
             Hex Integer Color  -- ^ Custom hexcolor fallback
             deriving (Eq, Show, Read)

data Styling = Bold | Italic | Underline | Fixed | Foreground Color | Background Color
               deriving (Eq,Show,Read)

toANSIColor :: Color -> ANSI.Colour
toANSIColor c =
  case c of
   Aqua       -> ANSI.Cyan
   Black      -> ANSI.Black 
   Blue       -> ANSI.Blue
   Fuchsia    -> ANSI.Magenta
   Gray       -> ANSI.Cyan
   Green      -> ANSI.Green
   Lime       -> ANSI.Green
   Maroon     -> ANSI.Red
   Navy       -> ANSI.Blue
   Olive      -> ANSI.Green
   Purple     -> ANSI.Red
   Red        -> ANSI.Red
   Silver     -> ANSI.Cyan
   Teal       -> ANSI.Green
   White      -> ANSI.White
   Yellow     -> ANSI.Yellow
   Hex  _ x   -> toANSIColor x

toCSSColor :: Color -> String
toCSSColor (Hex x _) = '#': printf "%6x" x
toCSSColor c = map toLower $ show c 

toANSIHighlight :: Styling -> ANSI.Highlight
toANSIHighlight s =
  case s of
    Bold         -> ANSI.Bold
    Italic       -> ANSI.Italic
    Underline    -> ANSI.Underscore
    Fixed        -> ANSI.Normal
    Foreground c -> ANSI.Foreground $ toANSIColor c
    Background c -> ANSI.Background $ toANSIColor c

colorful :: Style
colorful t =
  case t of
    Keyword   -> [Foreground Green, Underline]
    Symbol    -> []
    String    -> [Foreground Green]
    Char      -> [Foreground Red]
    Number    -> [Foreground Teal]
    Regex     -> [Foreground Maroon]
    Type      -> [Foreground Blue]
    Label     -> [Foreground Red, Underline]
    Preproc   -> [Foreground Blue, Underline]
    Function  -> [Foreground Blue, Bold]
    VarId     -> []
    ConId     -> [Foreground Blue]
    CBracket  -> [Foreground Red]
    Comment   -> [Foreground Gray]
    Selector  -> [Foreground Blue]
    Property  -> [Foreground Green, Underline]
    Tag       -> [Foreground Blue]
    Entity    -> [Foreground Green]
    Math      -> [Foreground Green]
    Alert     -> [Background Aqua]
    _         -> []

monochrome :: Style
monochrome t =
  case t of
    Keyword   -> [Underline]
    Symbol    -> []
    String    -> []
    Char      -> []
    Number    -> []
    Regex     -> []
    Type      -> []
    Label     -> [Underline]
    Preproc   -> [Underline]
    Function  -> [Bold]
    VarId     -> []
    ConId     -> []
    CBracket  -> []
    Comment   -> [Italic]
    Selector  -> []
    Property  -> [Underline]
    Tag       -> []
    Entity    -> []
    Math      -> []
    Alert     -> [Bold]
    _         -> []

toANSI :: Options -> Tokens -> String
toANSI opts = F.concatMap tokenToANSI . consolidate
 where tokenToANSI (t,s) = ANSI.highlight (map toANSIHighlight $ optStyle opts t) s

-- Use with \usepackage{fancyvrb} \usepackage[usenames,dvipsnames]{color}
toLaTeX :: Options -> Tokens -> String 
toLaTeX opts toks =
  concat [ "\\begin{Verbatim}[commandchars=\\\\\\{\\}]\n"
         , sourcelines
         , "\\end{Verbatim}" ]
    where sourcelines = F.concatMap tokenToLaTeX . consolidate $ toks
          tokenToLaTeX (t,s) = foldr addLaTeXHighlight (escapeForVerbatim s)
                                 (optStyle opts t)
          escapeForVerbatim "" = ""
          escapeForVerbatim ('\\':xs) =
             "{\\textbackslash}" ++ escapeForVerbatim xs
          escapeForVerbatim (c:xs) | c `elem` "{}" =
             '\\':c:escapeForVerbatim xs
          escapeForVerbatim (c:xs) = c:escapeForVerbatim xs

addLaTeXHighlight :: Styling -> String -> String
addLaTeXHighlight st x =
  case st of
    Bold      -> "\\textbf{" ++ x ++ "}"
    Italic    -> "\\textit{" ++ x ++ "}"
    Underline -> "\\underline{" ++ x ++ "}"
    Fixed     -> "\\texttt{" ++ x ++ "}"
    Foreground c -> toLaTeXColor False c x
    Background c -> toLaTeXColor True c x

toLaTeXColor :: Bool -> Color -> String -> String
toLaTeXColor background c s =
  cmd ++ col ++ "{" ++ s ++ "}"
    where inBr x = "{" ++ x ++ "}"
          cmd = if background then "\\colorbox" else "\\textcolor"
          col = case c of
                 Aqua     -> inBr "Aquamarine"
                 Lime     -> inBr "LimeGreen"
                 Navy     -> inBr "NavyBlue"
                 Olive    -> inBr "OliveGreen"
                 Silver   -> inBr "Cyan"
                 Teal     -> inBr "TealBlue"
                 Hex x _  -> "[rgb]{" ++ hexToRGB x ++ "}"
                 x        -> inBr (show x)

hexToRGB :: Integer -> String
hexToRGB x =
  printf "%0.2f,%0.2f,%0.2f" (toFrac r) (toFrac g) (toFrac b)
   where r = shiftR x 16 .&. 0xFF
         g = shiftR x 8 .&. 0xFF
         b = x .&. 0xFF

toFrac :: Integer -> Double
toFrac x = fromIntegral x / 256

toHtmlCSS :: Options -> Tokens -> [XHtml.Html]
toHtmlCSS _opts = map go . F.toList . consolidate
  where go (Whitespace, s) = XHtml.stringToHtml s
        go (Plain, s)      = XHtml.stringToHtml s
        go (x, s)          = XHtml.thespan XHtml.! [XHtml.theclass $ show x] XHtml.<< s

toHtmlCSSInline :: Options -> Tokens -> [XHtml.Html]
toHtmlCSSInline opts = map go . F.toList . consolidate
  where go (t, s) = let styles = map stylingToCSSProperty $ optStyle opts t
                    in  if null styles
                           then XHtml.stringToHtml s
                           else XHtml.thespan XHtml.! [XHtml.thestyle $ concat styles] XHtml.<< s

toHtmlInline :: Options -> Tokens -> [Html.Html]
toHtmlInline opts = map go . F.toList . consolidate
  where go (t, s) = foldl (flip ($)) (Html.stringToHtml s) (map stylingToHtmlTag $ optStyle opts t)

stylingToHtmlTag :: Styling -> Html.Html -> Html.Html
stylingToHtmlTag h =
  case h of
    Bold          -> Html.bold
    Italic        -> Html.italics
    Underline     -> Html.underline
    Fixed         -> id
    Foreground c  -> Html.font Html.! [Html.color $ toCSSColor c]
    Background c  -> Html.font Html.! [Html.bgcolor $ toCSSColor c]

stylingToCSSProperty :: Styling -> String
stylingToCSSProperty h =
  case h of
    Bold          -> "font-weight: bold;"
    Italic        -> "font-style: italic;" 
    Underline     -> "text-decoration: underline;"
    Fixed         -> "font-family: monospace;"
    Foreground c  -> "color: " ++ toCSSColor c ++ ";"
    Background c  -> "background-color: " ++ toCSSColor c ++ ";"

cssFor :: Options -> String
cssFor opts =
 "\n.sourceCode, .lineNumbers { margin: 0; padding: 0; border: 0; \ 
 \                              vertical-align: baseline; border: none; }\n\
 \td.lineNumbers { text-align: right; background-color: #EBEBEB; \
 \                 color: black; padding-right: 5px; padding-left: 5px; } \n\
 \td.sourceCode { padding-left: 5px; }\n" ++
 concatMap (\tokType -> "pre.sourceCode span." ++ show tokType ++ " { " ++
                        cssProps tokType ++ "}\n") allTokTypes
    where cssProps t = unwords $ map stylingToCSSProperty (optStyle opts t)
          allTokTypes = [ Whitespace
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
                        , Math
                        , Alert ]

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

