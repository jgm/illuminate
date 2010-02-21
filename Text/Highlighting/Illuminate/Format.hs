module Text.Highlighting.Illuminate.Format (Style, Styling(..), Color(..),
         colorful, monochrome, toANSI, toHtmlCSS, cssFor) where
import Text.Highlighting.Illuminate.Types
import qualified Language.Haskell.HsColour.ANSI as ANSI
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

-- | A Style is a generic instruction for formatting a token of the given
-- type.  The same style can be used for various output formats (HTML,
-- ANSI, LaTeX...).
type Style = TokenType -> [Styling]

-- | Colours supported by ANSI codes.
data Color = Aqua | Black | Blue | Fuchsia | Gray | Green | Lime | Maroon |
             Navy | Olive | Purple | Red | Silver | Teal | White | Yellow |
             Hex String Color  -- ^ Custom hexcolor fallback
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
toCSSColor (Hex x _) = '#':x
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
    Symbol    -> [Foreground Red]
    String    -> [Foreground Green]
    Char      -> [Foreground Red]
    Number    -> [Foreground Teal]
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
    Alert     -> [Bold]
    _         -> []

toANSI :: Style -> Tokens -> String
toANSI styl = F.concatMap tokenToANSI . consolidate
 where tokenToANSI (t,s) = ANSI.highlight (map toANSIHighlight $ styl t) s

toHtmlCSS :: Tokens -> [Html]
toHtmlCSS = F.toList . fmap go . consolidate
  where go (Whitespace, s) = stringToHtml s
        go (Plain, s)      = stringToHtml s
        go (x, s)          = thespan ! [theclass $ show x] << s

stylingToCSSProperty :: Styling -> String
stylingToCSSProperty h =
  case h of
    Bold          -> "font-weight: bold; "
    Italic        -> "font-style: italic; " 
    Underline     -> "text-decoration: underline; "
    Fixed         -> "font-family: monospace; "
    Foreground c  -> "color: " ++ toCSSColor c ++ "; "
    Background c  -> "background-color: " ++ toCSSColor c ++ "; "

cssFor :: Style -> String
cssFor colors =
 "\n.sourceCode, .lineNumbers { margin: 0; padding: 0; border: 0; \ 
 \                              vertical-align: baseline; border: none; }\n\
 \td.lineNumbers { text-align: right; background-color: #EBEBEB; \
 \                 color: black; padding-right: 5px; padding-left: 5px; } \n\
 \td.sourceCode { padding-left: 5px; }\n" ++
 concatMap (\tokType -> "pre.sourceCode span." ++ show tokType ++ " { " ++
                        cssProps tokType ++ "}\n") allTokTypes
    where cssProps t = concatMap stylingToCSSProperty (colors t)
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
                        , Alert ]

