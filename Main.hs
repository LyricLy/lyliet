import Parser
import System.Environment
import qualified Data.Text as T


main = do
  (fn:_) <- getArgs
  code <- readFile fn
  let p = parseMaybe lyliet (T.pack code)
  print p
