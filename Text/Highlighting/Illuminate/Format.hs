module Text.Highlighting.Illuminate.Format
           ( Options(..)
           , defaultOptions
           , Style
           , Styling(..)
           , Color(..)
           , colorful
           , hscolour
           , monochrome
           , toANSI
           , toLaTeX
           , toXHtmlCSS
           , toXHtmlInline
           , cssFor
           , toHtmlCSS
           , toHtmlInline) where
import Text.Highlighting.Illuminate.Types
import qualified Language.Haskell.HsColour.ANSI as ANSI
import Data.Sequence (empty, (<|))
import qualified Data.Foldable as F
import qualified Text.XHtml as X
import qualified Text.Html as H
import Data.Char (toLower)
import Data.Bits (shiftR, (.&.))
import Text.Printf (printf)

data Options = Options {
          optStyle       :: Style -- ^ Highlighting style
        , optNumberLines :: Bool  -- ^ Number lines?
        , optStartNumber :: Int   -- ^ Number of first line
        , optAnchors     :: Bool  -- ^ Add anchor with number as ID to each line
        }

defaultOptions :: Options
defaultOptions = Options { optStyle = colorful
                         , optNumberLines = False
                         , optStartNumber = 1
                         , optAnchors = False }

-- | A Style is a generic instruction for formatting a token of the given
-- type.  The same style can be used for various output formats (HTML,
-- ANSI, LaTeX...).
type Style = TokenType -> [Styling]

data Styling = Bold | Italic | Underline | Fixed | Foreground Color | Background Color
               deriving (Eq,Show,Read)

-- Styles

-- | A colorful style.
colorful :: Style
colorful t =
  case t of
    Keyword   -> [Foreground Green, Bold]
    Symbol    -> []
    String    -> [Foreground Green]
    Char      -> [Foreground Red]
    Number    -> [Foreground Teal]
    Regex     -> [Foreground Maroon]
    Type      -> [Foreground Blue]
    Label     -> [Foreground Red, Underline]
    Preproc   -> [Foreground Blue, Underline]
    Function  -> [Foreground Blue, Bold]
    Variable  -> [Foreground Maroon]
    VarId     -> []
    ConId     -> [Foreground Blue]
    CBracket  -> [Foreground Red]
    Comment   -> [Foreground Gray]
    Selector  -> [Foreground Blue, Bold]
    Property  -> [Foreground Green]
    Tag       -> [Foreground Blue]
    Entity    -> [Foreground Green]
    Math      -> [Foreground Green]
    NewFile   -> [Foreground Green]
    OldFile   -> [Foreground Red]
    Alert     -> [Background Aqua]
    _         -> []

-- | A style based on hscolour.
hscolour :: Style
hscolour t =
  case t of
    Keyword   -> [Foreground Green, Underline]
    Symbol    -> []
    String    -> [Foreground Fuchsia]
    Char      -> [Foreground Fuchsia]
    Number    -> [Foreground Fuchsia]
    Regex     -> [Foreground Fuchsia]
    Type      -> []
    Label     -> [Foreground Red, Underline]
    Preproc   -> [Foreground Blue, Underline]
    Function  -> [Foreground Blue]
    Variable  -> []
    VarId     -> []
    ConId     -> []
    CBracket  -> []
    Comment   -> [Foreground Gray]
    Selector  -> [Foreground Green, Underline]
    Property  -> [Foreground Blue]
    Tag       -> [Foreground Blue]
    Entity    -> [Foreground Green]
    Math      -> [Foreground Green]
    NewFile   -> [Foreground Green]
    OldFile   -> [Foreground Red]
    Alert     -> [Background Aqua]
    _         -> []

-- | A black and white style.
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
    Variable  -> []
    VarId     -> []
    ConId     -> []
    CBracket  -> []
    Comment   -> [Italic]
    Selector  -> []
    Property  -> [Underline]
    Tag       -> []
    Entity    -> []
    Math      -> []
    NewFile   -> []
    OldFile   -> []
    Alert     -> [Bold]
    _         -> []

-- Colors

data Color = Aqua | Black | Blue | Fuchsia | Gray | Green | Lime | Maroon |
             Navy | Olive | Purple | Red | Silver | Teal | White | Yellow |
             Hex Integer Color  -- ^ Second argument is a fallback color
             deriving (Eq, Show, Read)

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

toLaTeXColor :: Bool    -- ^ Background color?
             -> Color   -- ^ Color to use
             -> String  -- ^ String to colorize
             -> String
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

-- ANSI

-- | Highlight tokens using ANSI control sequences.
toANSI :: Options -> Tokens -> String
toANSI opts toks =
  if optNumberLines opts
     then unlines $ zipWith addNumber [startnum..] (lines source)
     else source
  where
   addNumber x s = ANSI.highlight [ANSI.Foreground ANSI.Cyan] $ fmtNumber x ++ s
   fmtNumber = printf ("%" ++ (show . length . show $ maxnum) ++ "d ")
   startnum = optStartNumber opts
   source = F.concatMap tokenToANSI . consolidate $ toks
   maxnum = startnum + length source
   tokenToANSI (t,s) = ANSI.highlight (map toANSIHighlight $ optStyle opts t) s

toANSIHighlight :: Styling -> ANSI.Highlight
toANSIHighlight s =
  case s of
    Bold         -> ANSI.Bold
    Italic       -> ANSI.Italic
    Underline    -> ANSI.Underscore
    Fixed        -> ANSI.Normal
    Foreground c -> ANSI.Foreground $ toANSIColor c
    Background c -> ANSI.Background $ toANSIColor c

-- LaTeX

-- | Highlight as LaTeX.
-- Use with
--
-- > \usepackage{fancyvrb}
-- > \usepackage[usenames,dvipsnames]{color}
--
toLaTeX :: Options -> Tokens -> String 
toLaTeX opts toks =
  concat [ "\\begin{Verbatim}[commandchars=\\\\\\{\\}"
         , numberlines
         , "]\n"
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
          numberlines = if optNumberLines opts
                           then ", numbers=left" ++
                                case optStartNumber opts of
                                      1 -> ""
                                      x -> ", firstnumber=" ++ show x
                           else ""

addLaTeXHighlight :: Styling -> String -> String
addLaTeXHighlight st x =
  case st of
    Bold      -> "\\textbf{" ++ x ++ "}"
    Italic    -> "\\textit{" ++ x ++ "}"
    Underline -> "\\underline{" ++ x ++ "}"
    Fixed     -> "\\texttt{" ++ x ++ "}"
    Foreground c -> toLaTeXColor False c x
    Background c -> toLaTeXColor True c x

-- HTML

-- | Highlight as XHTML with CSS classes.
toXHtmlCSS :: Options -> Tokens -> X.Html
toXHtmlCSS opts toks = addLineNums source
  where toklist            = F.toList . consolidate $ toks
        source             = addPre $ X.concatHtml $ map go toklist
        go (Whitespace, s) = X.stringToHtml s
        go (Plain, s)      = X.stringToHtml s
        go (x, s)          = X.thespan X.!
                                [X.theclass $ show x] X.<< s
        linecount          = length . lines . concatMap snd $ toklist
        addPre x           = X.pre X.! [X.theclass "sourceCode"] X.<< x
        minnum             = optStartNumber opts
        maxnum             = minnum + linecount - 1
        linenumsCell x y   = X.td X.! [X.theclass "lineNumbers"] X.<<
                                 addPre (map mkLinenum [x..y])
        mkLinenum x        = if optAnchors opts
                                then X.anchor X.! [X.identifier $ show x] X.<<
                                        (show x ++ "\n")
                                else X.stringToHtml (show x ++ "\n")
        mainCell x         = X.td X.! [X.theclass "sourceCode"] X.<< x
        addLineNums x      = if optNumberLines opts
                                then X.table $ X.tr X.<<
                                      [linenumsCell minnum maxnum, mainCell x]
                                else x

-- | Highlight as XHTML with inline styles.
toXHtmlInline :: Options -> Tokens -> X.Html
toXHtmlInline opts toks = addLineNums source
  where toklist            = F.toList . consolidate $ toks
        source             = addPre $ X.concatHtml $ map go toklist
        go (t, s) = let styles = map stylingToCSSProperty $ optStyle opts t
                    in  if null styles
                           then X.stringToHtml s
                           else X.thespan X.!
                                 [X.thestyle $ concat styles] X.<< s
        linecount          = length . lines . concatMap snd $ toklist
        addPre x           = X.pre X.! [X.thestyle "padding: 0;margin: 0;"] X.<< x
        minnum             = optStartNumber opts
        maxnum             = minnum + linecount - 1
        linenumstyle       = "text-align:right;border-right: 1px solid gray;" ++
                             "padding: 0 5px 0 5px;vertical-align: baseline;"
        linenumsCell x y   = X.td X.! [X.thestyle linenumstyle] X.<<
                                 addPre (map mkLinenum [x..y])
        mkLinenum x        = if optAnchors opts
                                then X.anchor X.! [X.identifier $ show x] X.<<
                                        (show x ++ "\n")
                                else X.stringToHtml (show x ++ "\n")
        mainCell x         = X.td X.! [X.thestyle "padding: 0 5px 0 5px;"] X.<< x
        addLineNums x      = if optNumberLines opts
                                then X.table $ X.tr X.<<
                                      [linenumsCell minnum maxnum, mainCell x]
                                else x

-- | Highlight as HTML with CSS classes.
toHtmlCSS :: Options -> Tokens -> H.Html
toHtmlCSS opts toks = addLineNums source
  where toklist            = F.toList . consolidate $ toks
        source             = addPre $ H.concatHtml $ map go toklist
        go (Whitespace, s) = H.stringToHtml s
        go (Plain, s)      = H.stringToHtml s
        go (x, s)          = H.thespan H.!
                                [H.theclass $ show x] H.<< s
        linecount          = length . lines . concatMap snd $ toklist
        addPre x           = H.pre H.! [H.theclass "sourceCode"] H.<< x
        minnum             = optStartNumber opts
        maxnum             = minnum + linecount - 1
        linenumsCell x y   = H.td H.! [H.theclass "lineNumbers"] H.<<
                                 addPre (map mkLinenum [x..y])
        mkLinenum x        = if optAnchors opts
                                then H.anchor H.! [H.identifier $ show x] H.<<
                                        (show x ++ "\n")
                                else H.stringToHtml (show x ++ "\n")
        mainCell x         = H.td H.! [H.theclass "sourceCode"] H.<< x
        addLineNums x      = if optNumberLines opts
                                then H.table $ H.tr H.<<
                                      [linenumsCell minnum maxnum, mainCell x]
                                else x

-- | Highlight as HTML with inline styles.
toHtmlInline :: Options -> Tokens -> H.Html
toHtmlInline opts toks = addLineNums source
  where toklist            = F.toList . consolidate $ toks
        source             = addPre $ H.concatHtml $ map go toklist
        go (t, s)          = foldl (flip ($)) (H.stringToHtml s)
                                (map stylingToHtmlTag $ optStyle opts t)
        linecount          = length . lines . concatMap snd $ toklist
        addPre x           = H.pre H.! [H.thestyle "padding: 0;margin: 0;"] H.<< x
        minnum             = optStartNumber opts
        maxnum             = minnum + linecount - 1
        linenumstyle       = "text-align:right;border-right: 1px solid gray;" ++
                             "padding: 0 5px 0 5px;vertical-align: baseline;"
        linenumsCell x y   = H.td H.! [H.thestyle linenumstyle] H.<<
                                 addPre (map mkLinenum [x..y])
        mkLinenum x        = if optAnchors opts
                                then H.anchor H.! [H.identifier $ show x] H.<<
                                        (show x ++ "\n")
                                else H.stringToHtml (show x ++ "\n")
        mainCell x         = H.td H.! [H.thestyle "padding: 0 5px 0 5px;"] H.<< x
        addLineNums x      = if optNumberLines opts
                                then H.table $ H.tr H.<<
                                      [linenumsCell minnum maxnum, mainCell x]
                                else x

stylingToHtmlTag :: Styling -> H.Html -> H.Html
stylingToHtmlTag h =
  case h of
    Bold          -> H.bold
    Italic        -> H.italics
    Underline     -> H.underline
    Fixed         -> id
    Foreground c  -> H.font H.! [H.color $ toCSSColor c]
    Background c  -> H.font H.! [H.bgcolor $ toCSSColor c]

stylingToCSSProperty :: Styling -> String
stylingToCSSProperty h =
  case h of
    Bold          -> "font-weight: bold;"
    Italic        -> "font-style: italic;" 
    Underline     -> "text-decoration: underline;"
    Fixed         -> "font-family: monospace;"
    Foreground c  -> "color: " ++ toCSSColor c ++ ";"
    Background c  -> "background-color: " ++ toCSSColor c ++ ";"

-- | CSS snippet appropriate for the specificed options.
cssFor :: Options -> String
cssFor opts =
 "\n.sourceCode, .lineNumbers { margin: 0; padding: 0; border: 0; \ 
 \                              vertical-align: baseline; border: none; }\n\
 \td.lineNumbers { text-align: right; border-right: 1px solid gray; \
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
                        , Variable
                        , VarId
                        , ConId
                        , CBracket
                        , Comment
                        , Selector
                        , Property
                        , Tag
                        , Entity
                        , Math
                        , NewFile
                        , OldFile
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

