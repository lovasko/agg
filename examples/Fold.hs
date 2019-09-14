import qualified Number.Aggregate as A
import qualified Data.Set as T
import qualified Data.Sequence as Q
import qualified Data.Foldable as F

main
  :: IO ()
main = do
  -- Create a list, set and a sequence of numbers.
  let l = [1.0, 2.0, 2.4, 1.2, 5.6, 5.0, 0.1, 4.4] :: [Double]
  let t = T.fromList l
  let q = Q.fromList l
  
  -- Compute the average and maximum from both data structures.
  let la = A.get $ F.foldr A.update A.average l
  let ta = A.get $ F.foldr A.update A.average t
  let qa = A.get $ F.foldr A.update A.average q
  let lm = A.get $ F.foldr A.update A.maximum l
  let tm = A.get $ F.foldr A.update A.maximum t
  let qm = A.get $ F.foldr A.update A.maximum q

  -- Print the results.
  putStr "lst avg = " >> putStrLn (maybe "not available" show la)
  putStr "set avg = " >> putStrLn (maybe "not available" show ta)
  putStr "seq avg = " >> putStrLn (maybe "not available" show qa)
  putStr "lst max = " >> putStrLn (maybe "not available" show lm)
  putStr "set max = " >> putStrLn (maybe "not available" show tm)
  putStr "seq max = " >> putStrLn (maybe "not available" show qm)
