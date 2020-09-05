import Parser
import System.Environment
import Data.Maybe
import Control.Monad
import qualified Data.Text.IO as TIO


main = do
  (fn:_) <- getArgs
  code <- TIO.readFile fn
  t <- parseLyliet code fn
  when (isJust t) $ do
    print t
