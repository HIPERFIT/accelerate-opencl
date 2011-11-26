module Main where

import Test.Framework
import qualified Map_Test
import qualified Fold_Test
import qualified ZipWith_Test

-- Use plain output format, to avoid terminal color annotations
main = do opts <- interpretArgsOrExit ["--plain"]
          defaultMainWithOpts [tests] opts

tests = testGroup "Accelerate OpenCL"
        [ Map_Test.tests
        , ZipWith_Test.tests
        , Fold_Test.tests
        ]

