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
    --CType, CMacro, 
    CUTranslSkel(..)
  )
  where

import Language.C
--import Text.PrettyPrint

--type CType        = [TypeSpec]
--type CMacro       = (Id, Maybe Exp)
data CUTranslSkel = CUTranslSkel [Definition] --[CMacro]
--                                 FilePath


instance Show CUTranslSkel where
  show (CUTranslSkel code) = header ++ (unlines $ map show code)

header :: String
header = "#include <accelerate_opencl_shape.h>\n\n"


-- instance Pretty CUTranslSkel where
--   pretty (CUTranslSkel code defs skel) =
--     vcat [ include "accelerate_cuda_extras.h"
--          , vcat (map macro defs)
--          , pretty code
--          , include skel
--          ]


-- include :: FilePath -> Doc
-- include hdr = text "#include <" <> text hdr <> text ">"

-- macro :: CMacro -> Doc
-- macro (d,v) = text "#define" <+> text (identToString d)
--                              <+> maybe empty (parens . pretty) v

