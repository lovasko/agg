import Text.Read
import System.IO
import qualified Number.Aggregate as A

-- | Read a Double number from the standard input stream.
readDouble
  :: IO Double -- ^ double number
readDouble = do
  putStr "> "
  line <- getLine
  case readMaybe line of
    Just num -> return num
    Nothing  -> putStrLn "error parsing the number" >> readDouble

-- | Read-Eval-Print loop.
loop
  :: A.Aggregate -- ^ average
  -> A.Aggregate -- ^ variance
  -> IO ()       -- ^ action
loop avg var = do
  -- | Prompt the user for the next number.
  num <- readDouble

  -- | Update the running aggregates.
  let avg' = A.update num avg
  let var' = A.update num var

  -- Inform the user about the updated aggregates.
  putStr "running average = "
  putStrLn (maybe "not available" show (A.get avg'))
  putStr "running variance = "
  putStrLn (maybe "not available" show (A.get var'))

  -- | Continue the REPL.
  loop avg' var'

main
  :: IO ()
main = hSetBuffering stdout NoBuffering >> loop A.average A.variance
