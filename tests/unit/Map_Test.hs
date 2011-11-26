module Map_Test (tests) where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Smart as Acc
import qualified Data.Array.Accelerate.OpenCL as Acc

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests = testGroup "map"
        [ testProperty "map (+1), Float" test_mapAddOne
        , testProperty "map square, Float" test_mapSquare
        , testProperty "map (uncurry (+)), Float" test_mapSumTuple
        ]

-- We must explicitly mention the types that we want QuickCheck to
-- generate as elements
test_mapAddOne :: [Float] -> Bool
test_mapAddOne = mkMapTest (+1) (+1)

test_mapSquare :: [Float] -> Bool
test_mapSquare = mkMapTest (\x -> x * x) (\x -> x * x)

test_mapSumTuple :: [(Float,Float)] -> Bool
test_mapSumTuple = mkMapTest (Prelude.uncurry (+)) (Acc.uncurry (+))

-- | Generic map test
--
-- Supply it with a regular function and an Accelerate function that
-- performs the same operations, to create a QuickCheckable property
mkMapTest :: (Eq b, Elt a, Elt b)
         => (a -> b) -> (Exp a -> Exp b) -> [a] -> Bool
mkMapTest _ _ [] = True
mkMapTest f f_acc xs = Prelude.map f xs == (Acc.toList $ Acc.run (doMap vector))
  where
    doMap  = Acc.map f_acc . Acc.use
    vector = Acc.fromList (Z :. length xs) xs
