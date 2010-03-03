{-# LANGUAGE CPP #-}
import Text.Highlighting.Illuminate
import System.Environment
import qualified Text.Html as H
import qualified Text.XHtml as X
import Control.Monad
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import Prelude
import System.IO (hPutStrLn)
#else
import Prelude hiding (putStr, putStrLn, readFile, getContents, writeFile)
import System.IO.UTF8
#endif
import System.IO (stderr)
import System.Exit
import Data.List (intercalate)
import Data.Char (toLower)
import System.Console.GetOpt

data Flag = Help
          | Fragment
          | Version
          | List
          | NumberLines
          | StartNum String
          | Outfile String
          | Syntax String
          | Style String
          | Format String 
          deriving (Eq, Show)

cmdOpts :: [OptDescr Flag]
cmdOpts =
  [ Option ['h'] ["help"] (NoArg Help)   "show usage message"
  , Option ['v'] ["version"] (NoArg Version)   "print version"
  , Option ['f'] ["fragment"] (NoArg Fragment)  "fragment, without document header"
  , Option ['l'] ["list"] (NoArg List)   "list available language syntaxes"
  , Option ['n'] ["number"] (NoArg NumberLines)  "number lines"
  , Option []    ["start"] (ReqArg StartNum "NUMBER")  "start numbering at"
  , Option ['o'] ["outfile"] (ReqArg Outfile "FILE")  "name of output file"
  , Option []    ["syntax"] (ReqArg Syntax "SYNTAX")  "specify language syntax to use"
  , Option ['s'] ["style"] (ReqArg Style "STYLE")  "specify highlighting style"
  , Option ['t'] ["to"] (ReqArg Format "FORMAT")  "to format [ansi|latex|html|xhtml|htmlcss|xhtmlcss]"
  ]

getSyntax :: [Flag] -> Maybe String
getSyntax [] = Nothing
getSyntax (Syntax s : _) = Just s
getSyntax (_:xs) = getSyntax xs

getStartNum :: [Flag] -> Int
getStartNum [] = 1
getStartNum (StartNum n : _) = read n
getStartNum (_:xs) = getStartNum xs

getOutfile :: [Flag] -> String
getOutfile [] = "-" 
getOutfile (Outfile f : _) = f
getOutfile (_:xs) = getOutfile xs

getFormat :: [Flag] -> String
getFormat [] = "ansi"
getFormat (Format f : _) = map toLower f
getFormat (_:xs) = getFormat xs

getStyle :: [Flag] -> String
getStyle [] = "colorful" 
getStyle (Style s : _) = map toLower s
getStyle (_:xs) = getStyle xs

toUnixLineEndings :: String -> String
toUnixLineEndings = filter (/='\r')

main :: IO ()
main = do
  (opts, fnames, errs) <- getArgs >>= return . getOpt Permute cmdOpts
  prg <- getProgName
  let usageHeader = prg ++ " [options] [files...]"
  
  when (not . null $ errs) $
    ioError (userError $ concat errs ++ usageInfo usageHeader cmdOpts)

  let showLexer l = putStrLn $ name l ++ " (" ++
                               intercalate ", " (aliases l) ++ "): " ++
                               intercalate ", " (filenames l) 
  when (List `elem` opts) $
     mapM_ showLexer lexers >> exitWith ExitSuccess

  when (Help `elem` opts) $
     hPutStrLn stderr (usageInfo usageHeader cmdOpts) >>
     exitWith (ExitFailure 1)

  when (Version `elem` opts) $
     -- TODO add version using cabal
     putStrLn (prg ++ " " ++ " -- (c) 2010 John MacFarlane") >> 
     exitWith ExitSuccess

  s <- case fnames of
          []    -> getContents
          (x:_) -> readFile x

  let lexer = case getSyntax opts of
                  Just x  -> lexerByName x
                  Nothing -> case fnames of
                                []    -> Nothing
                                (x:_) -> lexerByFilename x

  let (output, fname) = case getOutfile opts of
                              "-"   -> (putStr, "-")
                              f     -> (writeFile f, f)

  let numberlines = NumberLines `elem` opts
  let startnum = getStartNum opts
  let fragment = Fragment `elem` opts

  style <- case getStyle opts of
                 "colorful"   -> return colorful
                 "monochrome" -> return monochrome
                 x            -> hPutStrLn stderr ("Unknown style `" ++ x ++ "'") >>
                                 exitWith (ExitFailure 3)

  let options = defaultOptions { optStyle = style
                               , optNumberLines = numberlines
                               , optStartNumber = startnum }

  let inHtml includes x = if fragment
                             then foldr (H.renderHtml' 0) "" (H.getHtmlElements x)
                             else H.renderHtml $ H.header H.<< includes H.+++ H.body H.<< x
  let inXHtml includes x = if fragment
                              then X.showHtmlFragment x
                              else X.showHtml $ X.header X.<< includes X.+++ X.body X.<< x

  let addLaTeXHeadFoot x = if fragment then x else unlines
                           [ "\\documentclass{report}"
                           , "\\usepackage{fancyvrb}"
                           , "\\usepackage[usenames,dvipsnames]{color}"
                           , "\\begin{document}"
                           , x
                           , "\\end{document}" ]

  let tokens = case tokenize lexer $ toUnixLineEndings s of
                    Right toks  -> toks
                    Left err    -> error $ show err

  case getFormat opts of
         "html"       -> output $ inHtml [H.thetitle H.<< fname] $
                         toHtmlInline options tokens
         "xhtml"      -> output $ inXHtml [X.thetitle X.<< fname] $
                         toXHtmlInline options tokens
         "htmlcss"    -> output $ inHtml [ H.thetitle H.<< fname
                                         , H.style H.! [H.thetype "text/css"] H.<<
                                             cssFor options ] $
                         toHtmlCSS options tokens
         "xhtmlcss"   -> output $ inXHtml [ X.thetitle X.<< fname
                                          , X.style X.! [X.thetype "text/css"] X.<<
                                             cssFor options ] $
                         toXHtmlCSS options tokens
         "latex"      -> output $ addLaTeXHeadFoot $ toLaTeX options tokens
         "ansi"       -> output $ toANSI options tokens
         x            -> hPutStrLn stderr ("Unknown format `" ++ x ++ "'") >>
                         exitWith (ExitFailure 5)

