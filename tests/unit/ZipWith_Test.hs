module ZipWith_Test (tests) where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Smart as Acc
import qualified Data.Array.Accelerate.OpenCL as Acc

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests = testGroup "zipWith"
        [ testProperty "zipWith (+), Float Float" test_zipWithAdd
--        , testProperty "zipWith mkTuple, (Float, Int)" test_zipWithTup
        ]

-- We must explicitly mention the types that we want QuickCheck to
-- generate as elements
test_zipWithAdd :: [Float] -> [Float] -> Bool
test_zipWithAdd = mkZipWithTest (+) (+)

-- test_zipWithTup :: [Float] -> [Int] -> Bool
-- test_zipWithTup = mkZipWithTest (\x y -> (x, y)) (\x y -> (x, y))

-- | Generic zipWith test
--
-- Supply it with a regular function and an Accelerate function that
-- performs the same operations, to create a QuickCheckable property
mkZipWithTest :: (Eq c, Elt a, Elt b, Elt c)
              => (a -> b -> c) -> (Exp a -> Exp b -> Exp c) -> [a] -> [b] -> Bool
mkZipWithTest _ _ [] _ = True
mkZipWithTest _ _ _ [] = True
mkZipWithTest f f_acc xs ys = Prelude.zipWith f xs ys == (Acc.toList run_zipWith)
  where
    run_zipWith = Acc.run (Acc.zipWith f_acc vector_xs vector_ys)
    vector_xs   = Acc.use $ Acc.fromList (Z :. length xs) xs
    vector_ys   = Acc.use $ Acc.fromList (Z :. length ys) ys
