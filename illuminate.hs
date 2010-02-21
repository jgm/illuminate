{-# LANGUAGE CPP #-}
import Text.Highlighting.Illuminate
import System.Environment
import Text.XHtml
import Control.Monad
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import Prelude
import System.IO (hPutStrLn)
#else
import Prelude hiding (putStr, readFile)
import System.IO.UTF8
#endif
import System.IO (stderr)
import System.Exit
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  let isOpt ('-':_) = True
      isOpt _       = False
  let opts = filter isOpt args
  let fnames = filter (not . isOpt) args
 
  when (null fnames) $ usageAndExit

  let file = head fnames
  let lang' = dropWhile (=='.') $ takeExtension file
  s <- readFile file
  let tokens = case tokenize (lexerByExtension lang') s of
                    Right toks  -> toks
                    Left err    -> error $ show err
  let style' = if "-mono" `elem` opts
                  then monochrome
                  else colorful
  if "-html" `elem` opts
     then putStr $ showHtml $
                   pre ! [theclass "sourceCode"] << toHtmlCSSInline style' tokens 
     else putStr $ toANSI style' tokens

usageAndExit :: IO ()
usageAndExit = do
  prog <- getProgName
  hPutStrLn stderr $ "Usage:  " ++ prog ++ " [-html|-ansi] file"
  exitWith $ ExitFailure 1

