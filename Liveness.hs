{-# LANGUAGE NamedFieldPuns #-}

module Liveness (livenessAction) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import Futhark.IR.KernelsMem (KernelsMem, MemOp (..), Prog (..), freeIn, typeOf)
import Futhark.IR.Prop.Names (Names, boundByStm, boundInBody, namesFromList, namesSubtract)
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Pipeline

gen :: Stm KernelsMem -> Names
gen stm@(Let {stmExp}) =
  freeIn stmExp

kill :: Stm KernelsMem -> Names
kill stm@Let {stmPattern} =
  freeIn stmPattern <> boundByStm stm

inOut :: (Stm KernelsMem, (Names, Names)) -> (Names, Names) -> (Names, Names)
inOut (stm@Let {stmPattern, stmExp = If cond then_branch else_branch ifdec}, (i, o)) (i_prev, o_prev) =
  let (_, then_liveness) = unzip $ liveness then_branch
      (_, else_liveness) = unzip $ liveness else_branch
      branch_outs = (fst $ head then_liveness) <> (fst $ head else_liveness)
      i' = freeIn cond <> (branch_outs `namesSubtract` (freeIn stmPattern <> boundByStm stm))
      o' = o <> i_prev
   in (i', o')
inOut (stm@Let {stmPattern, stmExp = DoLoop ctx vals form body}, (i, o)) (i_prev, o_prev) =
  let (_, body_liveness) = unzip $ liveness body
      body_outs = (fst $ head body_liveness)
      i' =
        freeIn (map snd ctx)
          <> freeIn (map snd vals)
          <> ( body_outs
                 `namesSubtract` ( freeIn stmPattern
                                     <> boundByStm stm
                                     <> freeIn (map fst vals)
                                     <> freeIn (map fst ctx)
                                     <> namesFromList (map (paramName . fst) (ctx <> vals))
                                 )
             )
      o' = o <> i_prev
   in (i', o')
inOut (stm@Let {stmPattern, stmExp = BasicOp bop}, (i, o)) (i_prev, o_prev) =
  let o' = o <> i_prev
      i' = gen stm <> (o' `namesSubtract` kill stm)
   in (i', o')
inOut (stm@Let {stmPattern, stmExp = Apply name args rets aux}, (i, o)) (i_prev, o_prev) =
  let o' = o <> i_prev
      i' = gen stm <> (o' `namesSubtract` kill stm)
   in (i', o')
inOut (stm, (i, o)) (i_prev, o_prev) =
  let o' = o <> i_prev
      i' = gen stm <> (o' `namesSubtract` kill stm)
   in (i', o')

liveness :: Body KernelsMem -> [(Stm KernelsMem, (Names, Names))]
liveness Body {bodyStms, bodyResult} =
  fix (zip (toList bodyStms) . scanr inOut (mempty, mempty)) $
    zip (toList bodyStms) $
      zip (repeat mempty) (replicate (length bodyStms - 1) mempty ++ [freeIn bodyResult])

livenessStm :: Stm KernelsMem -> FutharkM ()
livenessStm (Let {stmPattern, stmAux, stmExp}) =
  undefined

livenessFun :: FunDef KernelsMem -> FutharkM ()
livenessFun
  FunDef
    { funDefEntryPoint,
      funDefName,
      funDefParams,
      funDefBody = body@Body {bodyDec, bodyStms, bodyResult}
    } = do
    liftIO $ putStrLn $ "Analyzing " ++ show funDefName
    liftIO $
      putStrLn $
        unwords
          [ "Params:",
            show funDefParams,
            "\nBodyDec:",
            show bodyDec,
            "\nBodyResult:",
            show bodyResult
          ]
    liftIO $
      putStrLn $
        unlines $
          toList $
            fmap (\(stm, (i, o)) -> pretty stm ++ "   : gen=" ++ pretty (gen stm) ++ ", kill=" ++ pretty (kill stm) ++ ", in=" ++ pretty i ++ ", out=" ++ pretty o) $
              liveness body

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x
   in if x' == x then x else fix f x'

livenessProg :: Prog KernelsMem -> FutharkM ()
livenessProg (Prog _ funs) = mapM_ livenessFun funs

livenessAction :: Action KernelsMem
livenessAction =
  Action
    { actionName = "memory allocation liveness analysis",
      actionDescription = "Perform liveness analysis on memory allocations",
      actionProcedure = livenessProg
    }
