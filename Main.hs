module Main where

import Control.Category ((>>>))
import Futhark.Actions (printAction)
import Futhark.Compiler
  ( newFutharkConfig,
    runCompilerOnProgram,
  )
import Futhark.IR.Kernels (Kernels)
import Futhark.IR.KernelsMem (KernelsMem)
import Futhark.IR.SOACS
import qualified Futhark.Pass.ExplicitAllocations.Kernels as Kernels
import Futhark.Pass.Simplify
import Futhark.Passes (kernelsPipeline)
import Futhark.Pipeline
import GHC.IO.Encoding (setLocaleEncoding)
import Liveness
import System.Environment (getArgs)
import System.IO

pipeline :: Pipeline SOACS KernelsMem
pipeline =
  kernelsPipeline
    >>> onePass Kernels.explicitAllocations
    >>> passes
      [ simplifyKernelsMem
      ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  setLocaleEncoding utf8
  args <- getArgs

  runCompilerOnProgram
    newFutharkConfig
    pipeline
    livenessAction
    (head args)
