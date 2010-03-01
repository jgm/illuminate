{-# LANGUAGE CPP #-}
import Text.Highlighting.Illuminate
import System.Environment
import qualified Text.Html as Html
import qualified Text.XHtml as XHtml
import Control.Monad
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import Prelude
import System.IO (hPutStrLn)
#else
import Prelude hiding (putStr, putStrLn, readFile)
import System.IO.UTF8
#endif
import System.IO (stderr)
import System.Exit
import Data.List (intercalate, find, isPrefixOf)

main :: IO ()
main = do
  args <- getArgs
  let isOpt ('-':_) = True
      isOpt _       = False
  let opts = filter isOpt args
  let fnames = filter (not . isOpt) args
  let showLexer l = putStrLn $ name l ++ " (" ++
        intercalate ", " (aliases l) ++ "): " ++
        intercalate ", " (filenames l) 
  when ("-list" `elem` opts) $ mapM_ showLexer lexers >> exitWith ExitSuccess
  
  when (null fnames) $ usageAndExit

  let file = head fnames
  s <- readFile file
  let tokens = case tokenize (lexerByFilename file) s of
                    Right toks  -> toks
                    Left err    -> error $ show err
  let numberlines = "-number" `elem` opts
  let startnum = case find ("-startnum=" `isPrefixOf`) opts of
                      Just x -> read $ drop 10 x
                      Nothing -> 1
  let defOptions = defaultOptions { optNumberLines = numberlines
                                  , optStartNumber = startnum }
  let options = if "-mono" `elem` opts
                   then defOptions{ optStyle = monochrome }
                   else defOptions{ optStyle = colorful }
  putStr $ if "-html" `elem` opts
              then Html.renderHtml $ Html.pre Html.! [Html.theclass "sourceCode"] Html.<<
                         toHtmlInline options tokens 
              else if "-xhtml" `elem` opts
                   then if "-css" `elem` opts
                        then XHtml.showHtml $ XHtml.style XHtml.<< cssFor options XHtml.+++
                                 XHtml.pre XHtml.! [XHtml.theclass "sourceCode"] XHtml.<<
                                 toHtmlCSS options tokens
                        else XHtml.showHtml $ XHtml.pre XHtml.! [XHtml.theclass "sourceCode"] XHtml.<<
                                 toHtmlCSSInline options tokens
                   else if "-latex" `elem` opts
                        then addLaTeXHeadFoot $ toLaTeX options tokens
                        else toANSI options tokens

addLaTeXHeadFoot :: String -> String
addLaTeXHeadFoot s = unlines
  [ "\\documentclass{report}"
  , "\\usepackage{fancyvrb}"
  , "\\usepackage[usenames,dvipsnames]{color}"
  , "\\begin{document}"
  , s
  , "\\end{document}" ]

usageAndExit :: IO ()
usageAndExit = do
  prog <- getProgName
  hPutStrLn stderr $ "Usage:  " ++ prog ++ " [-html|-ansi|-xhtml|-latex] [-css] [-mono] [-list] [-number] [-startnum=n] file"
  exitWith $ ExitFailure 1

