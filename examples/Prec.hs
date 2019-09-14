import Numeric.Half
import Data.Number.BigFloat
import qualified Number.Aggregate as A
import qualified Data.Foldable as F

main
  :: IO ()
main = do
  let f = [1.0, 2.0, 3.0, 4.0, 5.0, 6.7] :: [Float]
  let d = [1.0, 2.0, 3.0, 4.0, 5.0, 6.7] :: [Double]
  let h = [1.0, 2.0, 3.0, 4.0, 5.0, 6.7] :: [Half]
  let b = [1.0, 2.0, 3.0, 4.0, 5.0, 6.7] :: [BigFloat Prec50]

  let fa = A.get $ F.foldr A.update A.variance f
  let da = A.get $ F.foldr A.update A.variance d
  let ha = A.get $ F.foldr A.update A.variance h
  let ba = A.get $ F.foldr A.update A.variance b

  putStr "float    = " >> putStrLn (maybe "not available" show fa)
  putStr "double   = " >> putStrLn (maybe "not available" show da)
  putStr "half     = " >> putStrLn (maybe "not available" show ha)
  putStr "bigfloat = " >> putStrLn (maybe "not available" show ba)
