import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks {
      confHook = myConfHook }

myConfHook info flags = do
  local <- (confHook simpleUserHooks) info flags
  return $ local{ withPrograms = userSpecifyArgs "alex" ["-t","alex","--ghc"] $ withPrograms local } 

