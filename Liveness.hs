{-# LANGUAGE NamedFieldPuns #-}

module Liveness (livenessAction) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.IR.KernelsMem (KernelsMem, Prog (..), typeOf)
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Pipeline

livenessFun :: FunDef KernelsMem -> FutharkM ()
livenessFun
  FunDef
    { funDefEntryPoint,
      funDefName,
      funDefParams,
      funDefBody = Body {bodyDec, bodyStms, bodyResult}
    } = do
    liftIO $ putStrLn $ "Analyzing " ++ pretty funDefName
    liftIO $
      putStrLn $
        unwords
          [ "Params:",
            pretty funDefParams,
            "\nBodyDec:",
            pretty bodyDec,
            "\nBodyResult:",
            pretty bodyResult
          ]

livenessProg :: Prog KernelsMem -> FutharkM ()
livenessProg (Prog _ funs) = mapM_ livenessFun funs

livenessAction :: Action KernelsMem
livenessAction =
  Action
    { actionName = "memory allocation liveness analysis",
      actionDescription = "Perform liveness analysis on memory allocations",
      actionProcedure = livenessProg
    }
