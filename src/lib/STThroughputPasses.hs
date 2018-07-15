module STThroughputPasses where
import STTypes
import STAST
import STMetrics
import STAnalysis
import STComposeOps

-- given a LB's pxPerClock, its image dimesions, and a multiple to speed up,
-- speed up the pxPerCLock from inner most to outer most.
-- Returns the new pxPerClock (in reverse order of LB) and the amount sped up
-- NOTE: Unlike for a LB, here the pxPerClock and img dimensions must have the
-- inner most dimension come first this is the reverse of what it is on the linebuffer
increaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if mult is down to 1 or no more dimensions to speed up, return remaining pxPerClock 
increaseLBPxPerClock [] _ _ = ([], 1)
increaseLBPxPerClock p img mult | mult == 1 = (p, 1)
-- if not filling out this dimension, pInner * mult must divide into imgInner
-- no need to recurse further as done filling out dimensions
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner > pInner * mult && (imgInner `mod` (pInner * mult) == 0) =
  ((pInner * mult) : pTl, mult)
-- if filling out this dimension, imgInner must divide into mult * pInner cleanly
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner <= pInner * mult && ((mult * pInner) `mod` imgInner == 0) =
  (imgInner : pTl, mult * multOuter)
  where
    -- since requiring pInner to always divide into imgInner, this is ok
    remainingMultForOuterDims = mult `ceilDiv` (imgInner `ceilDiv` pInner)
    (pOuter, multOuter) = increaseLBPxPerClock pTl imgTl remainingMultForOuterDims
increaseLBPxPerClock p _ _ = (p, 1)

-- helper function for linebuffer speedUpIfPossible, goes through, increasing
-- parallelism of each component take an op and make it run x times faster
-- without wrapping it, where x is the second argument.
-- This may not be possible for some ops without wrapping them in a map
-- so the pair returns a best effort speed up op and the amount it was sped up
--
-- If its combinational, pretty much always just wrap it in map, as if later
-- speed up again, will just increase multiple on map, never see this again
speedUpIfPossible :: Int -> Op -> (Op, Int)
speedUpIfPossible throughMult op@(Add _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Sub _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Mul _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Div _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Max _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Min _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Ashr _ _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Shl _ _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Abs _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Not _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(And _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(Or  _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(XOr _) = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Eq = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Neq = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Lt = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Leq = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Gt = (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@Geq = (MapOp throughMult op, throughMult)
-- should it be possible to speed this up? Should I always just speed up by
-- wrapping in an array instead of increasing a wrapping array length if it exists?
-- OLD REASONING - no, can't speed this up or slow it down as don't want to
-- change the meaning of the data it reads every clock.
-- Always reading a token each clock, map to speed up
-- NEW REASONING - the no is wrong. Going to change this to just reading in
-- arrays of ints or bits, as that is what memory is. memory is a 1d array,
-- so these will just make parent array longer or wrap int/bit in array
speedUpIfPossible throughMult op@(MemRead (T_Array n t)) =
  (MemRead (T_Array (n*throughMult) t), throughMult)
speedUpIfPossible throughMult op@(MemRead t) =
  (MemRead (T_Array throughMult t), throughMult)
speedUpIfPossible throughMult op@(MemWrite (T_Array n t)) =
  (MemWrite (T_Array (n*throughMult) t), throughMult)
speedUpIfPossible throughMult op@(MemWrite t) =
  (MemWrite (T_Array throughMult t), throughMult)
-- NOTE: assuming that all pxPerClock are 1 unless inner dims pxPerClock ==
-- inner img dims
-- NOTE: for all dimensions, img dimension % pxPerClock == 0
-- NOTE: This works by speeding up inner dimensions before outer ones. Only
-- works if throughMult satisfies two conditions:
-- 1. Amount to make each full throuhgput dimension go from current pxPerClock
-- to full throughput divides cleanly into the throughputMult.
-- Stated rigorously: all i where i is number of full throuhgput dimensions:
--    ((\Pi_(0 to i-1) pxPerClock dim i) * throuhgMult) % (\Pi_(0 to i-1) product of full throuhgput dims) == 0
-- 2. After making all inner dimensions full throuhgput, the first dimension not
-- made full throughput must consume the rest of throuhgputMult and result in a
-- new pxPerClock that cleanly divides into that dimension.
-- Stated rigorously:
--    (first not full throuhgput dim) % ((product of all pxPerClock * throuhgMult) / (product of full throuhgput dims)) == 0
speedUpIfPossible throughMult (LineBuffer p w img t) =
  (LineBuffer (reverse reversedNewP) w img t, actualMult)
  where
    (reversedNewP, actualMult) =
      increaseLBPxPerClock (reverse p) (reverse img) throughMult
speedUpIfPossible throughMult (Constant_Int constArr) =
  (Constant_Int $ foldl (++) [] $ replicate throughMult constArr, throughMult)
speedUpIfPossible throughMult (Constant_Bit constArr) =
  (Constant_Bit $ foldl (++) [] $ replicate throughMult constArr, throughMult)
-- not going to change SLen in as, if 1, don't want to cause it to become
-- fractional
speedUpIfPossible throughMult (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) t) =
  (SequenceArrayRepack (sLenIn, oldArrLenOut * throughMult)
    (sLenOut, oldArrLenOut * throughMult) t, throughMult)
speedUpIfPossible throughMult op@(ArrayReshape _ _) =
  (MapOp throughMult op, throughMult)
speedUpIfPossible throughMult op@(DuplicateOutputs _ _) =
  (MapOp throughMult op, throughMult)

-- is this an ok inverse of our decision to only underutil for slowdown of seq operators?
speedUpIfPossible throughMult (MapOp par innerOp) | isComb innerOp =
  (MapOp (par*throughMult) innerOp, throughMult)
speedUpIfPossible throughMult (MapOp par innerOp) =
  let (spedUpInnerOp, actualMult) = speedUpIfPossible throughMult innerOp
  in (MapOp par spedUpInnerOp, actualMult)

-- only do it if less than full parallel and numComb % par == 0 or
-- more than paralle and par % numComb == 0
-- and if everything inside is comb, otherwise just speed up inside
speedUpIfPossible throughMult (ReduceOp par numComb innerOp) | isComb innerOp &&
  (((newPar <= numComb) && ((numComb `mod` newPar) == 0)) ||
  ((newPar > numComb) && ((newPar `mod` numComb) == 0))) =
  (ReduceOp newPar numComb innerOp, throughMult)
  where newPar = par*throughMult
speedUpIfPossible throughMult (ReduceOp par numComb innerOp) =
  let (spedUpInnerOp, actualMult) = speedUpIfPossible throughMult innerOp
  in (ReduceOp par numComb spedUpInnerOp, actualMult)

-- modify underutil if mult divides cleanly into underutil's denominator
-- or if removing underutil and the denominator divides cleanly into mult
speedUpIfPossible throughMult (Underutil denom op) |
  (denom `mod` throughMult) == 0 =
  (Underutil (denom `ceilDiv` throughMult) op, throughMult)
speedUpIfPossible throughMult (Underutil denom op) |
  (throughMult `mod` denom) == 0 =
  (spedUpOp, denom*innerMult)
  where
    remainingMult = throughMult `ceilDiv` denom
    (spedUpOp, innerMult) = speedUpIfPossible remainingMult op 
speedUpIfPossible throughMult op@(Underutil _ _) = speedUpIfPossible throughMult op

-- NOTE: what to do if mapping over a reg delay? Nothing? its sequential but,
-- unlike other sequential things like reduce, linebuffer its cool to duplicate
speedUpIfPossible throughMult (RegDelay d innerOp) =
  (RegDelay d spedUpInnerOp, innerMult)
  where (spedUpInnerOp, innerMult) = speedUpIfPossible throughMult innerOp 

speedUpIfPossible throughMult (ComposePar ops) = 
  let
    spedUpOpsAndMults = map (speedUpIfPossible throughMult) ops
    spedUpOps = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  in (ComposePar spedUpOps, minimum actualMults)
speedUpIfPossible throughMult (ComposeSeq ops) = 
  let
    spedUpOpsAndMults = map (speedUpIfPossible throughMult) ops
    (hdSpedUpOps:tlSpedUpOps) = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  -- doing a fold here instead of just making another composeSeq to make sure all
  -- ports still match 
  in (foldl (|>>=|) hdSpedUpOps tlSpedUpOps, minimum actualMults)
speedUpIfPossible _ op@(ComposeFailure _ _) = (op, 1)

speedUp throughMult op | actualMult == throughMult = spedUpOp
  where (spedUpOp, actualMult) = speedUpIfPossible throughMult op
speedUp throughMult op = ComposeFailure (BadThroughputMultiplier throughMult actualMult) (op, ComposeSeq [])
  where (spedUpOp, actualMult) = speedUpIfPossible throughMult op
