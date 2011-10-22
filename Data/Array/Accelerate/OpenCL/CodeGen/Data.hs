-- |
-- Module      : Data.Array.Accelerate.OpenCL.CodeGen.Data
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Common data types for code generation
--

module Data.Array.Accelerate.OpenCL.CodeGen.Data
  (
    CUTranslSkel(..)
  )
  where

import Language.C


data CUTranslSkel = CUTranslSkel [Definition]



instance Show CUTranslSkel where
  show (CUTranslSkel code) =
    header
      ++ (unlines $ map show code)

header :: String
header = "#pragma OPENCL EXTENSION cl_amd_printf : enable\n"
      ++ "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n\n"
      ++ include "accelerate_opencl_shape.h"


include :: FilePath -> String
include hdr = "#include <" ++ hdr ++ ">\n"

-- instance Pretty CUTranslSkel where
--   pretty (CUTranslSkel code defs skel) =
--     vcat [ include "accelerate_cuda_extras.h"
--          , vcat (map macro defs)
--          , pretty code
--          , include skel
--          ]


-- macro :: CMacro -> Doc
-- macro (d,v) = text "#define" <+> text (identToString d)
--                              <+> maybe empty (parens . pretty) v

