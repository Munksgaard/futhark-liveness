{-# LANGUAGE NamedFieldPuns #-}

module Liveness (livenessAction) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import Futhark.IR.KernelsMem (KernelsMem, MemOp (..), Prog (..), typeOf)
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Pipeline

genSubExp :: SubExp -> Set VName
genSubExp (Var vname) = Set.singleton vname
genSubExp (Constant _) = Set.empty

genBop :: BasicOp -> Set VName
genBop (SubExp sexp) = genSubExp sexp
genBop (Opaque sexp) =
  -- TODO: Is this wrong? It's opaque, so perhaps it should not be included
  -- here?
  genSubExp sexp
genBop (ArrayLit sexps _) = Set.unions $ fmap genSubExp sexps
genBop (UnOp _ sexp) = genSubExp sexp
genBop (BinOp _ sexp1 sexp2) = genSubExp sexp1 `Set.union` genSubExp sexp2
genBop (CmpOp _ sexp1 sexp2) = genSubExp sexp1 `Set.union` genSubExp sexp2
genBop (ConvOp _ sexp) = genSubExp sexp
genBop (Assert sexp _ _) = genSubExp sexp
genBop (Index vname slice) =
  Set.singleton vname
    `Set.union` foldMap (foldMap genSubExp) slice
genBop (Update vname slice sexp) =
  Set.singleton vname
    `Set.union` foldMap (foldMap genSubExp) slice
    `Set.union` genSubExp sexp
genBop (Concat _ vname vnames sexp) =
  Set.singleton vname
    `Set.union` Set.fromList vnames
    `Set.union` genSubExp sexp
genBop (Copy vname) = Set.singleton vname
genBop (Manifest _ vname) = Set.singleton vname
genBop (Iota sexp1 sexp2 sexp3 _) =
  Set.unions $ fmap genSubExp [sexp1, sexp2, sexp3]
genBop (Replicate shape sexp) =
  genSubExp sexp
    `Set.union` foldMap genSubExp (shapeDims shape)
genBop (Scratch _ sexps) = foldMap genSubExp sexps
genBop (Reshape shape_change vname) =
  Set.singleton vname
    `Set.union` foldMap (foldMap genSubExp) shape_change
genBop (Rearrange _ vname) = Set.singleton vname
genBop (Rotate sexps vname) =
  Set.singleton vname
    `Set.union` foldMap genSubExp sexps

genBody :: Body KernelsMem -> Set VName
genBody Body {bodyStms, bodyResult} =
  foldMap gen bodyStms
    `Set.union` foldMap genSubExp bodyResult

genLoopForm :: LoopForm KernelsMem -> Set VName
genLoopForm (ForLoop vname _ sexp params) =
  Set.singleton vname
    `Set.union` genSubExp sexp
    `Set.union` foldMap (Set.singleton . snd) params

genExp :: Exp KernelsMem -> Set VName
genExp (BasicOp bop) = genBop bop
genExp (Apply _ sexps_and_diets _ _) = foldMap (genSubExp . fst) sexps_and_diets
genExp (If sexp then_body else_body _) =
  genSubExp sexp
    `Set.union` genBody then_body
    `Set.union` genBody else_body
genExp (DoLoop ctx vals form body) =
  genBody body
    `Set.union` foldMap (genSubExp . snd) ctx
    `Set.union` foldMap (genSubExp . snd) vals
genExp (Op (Alloc sexp _)) = genSubExp sexp
genExp (Op (Inner _)) = Set.empty -- TODO

gen :: Stm KernelsMem -> Set VName
gen stm@(Let {stmExp}) =
  genExp stmExp

kill :: Stm KernelsMem -> Set VName
kill Let {stmPattern} =
  Set.union
    (Set.fromList $ fmap patElemName $ patternContextElements stmPattern)
    (Set.fromList $ fmap patElemName $ patternValueElements stmPattern)

livenessStm :: Stm KernelsMem -> FutharkM ()
livenessStm (Let {stmPattern, stmAux, stmExp}) =
  undefined

livenessFun :: FunDef KernelsMem -> FutharkM ()
livenessFun
  FunDef
    { funDefEntryPoint,
      funDefName,
      funDefParams,
      funDefBody = Body {bodyDec, bodyStms, bodyResult}
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
    liftIO $ putStrLn $ unlines $ toList $ fmap (\stm -> pretty stm ++ "   : gen=" ++ pretty (gen stm)) bodyStms

livenessProg :: Prog KernelsMem -> FutharkM ()
livenessProg (Prog _ funs) = mapM_ livenessFun funs

livenessAction :: Action KernelsMem
livenessAction =
  Action
    { actionName = "memory allocation liveness analysis",
      actionDescription = "Perform liveness analysis on memory allocations",
      actionProcedure = livenessProg
    }
