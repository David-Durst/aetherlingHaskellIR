module STThroughputPasses where
import STTypes
import STAST
import STMetrics
import STAnalysis
import STComposeOps

-- NOTE: AM I PUSHING TOO MUCH LOGIC INTO THE LEAVES BY HAVING THEM
-- UNDERUTIL THEMSELVES? SHOULD THE COMPOSESEQs DO UNDERUTIL WHEN CHILDREN CAN'T?

speedUp throughMult op | actualMult == throughMult = spedUpOp
  where (spedUpOp, actualMult) = attemptSpeedUp throughMult op
speedUp throughMult op = ComposeFailure
  (BadThroughputMultiplier throughMult actualMult) (op, ComposeSeq [])
  where (spedUpOp, actualMult) = attemptSpeedUp throughMult op

--
-- If its combinational, pretty much always just wrap it in map, as if later
-- speed up again, will just increase multiple on map, never see this again
attemptSpeedUp :: Int -> Op -> (Op, Int)
attemptSpeedUp throughMult op@(Add _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Sub _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Mul _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Div _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Max _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Min _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Ashr _ _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Shl _ _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Abs _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Not _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(And _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Or  _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(XOr _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Eq = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Neq = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Lt = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Leq = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Gt = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@Geq = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(LUT _) = (MapOp throughMult op, throughMult)
-- should it be possible to speed this up? Should I always just speed up by
-- wrapping in an array instead of increasing a wrapping array length if it exists?
-- OLD REASONING - no, can't speed this up or slow it down as don't want to
-- change the meaning of the data it reads every clock.
-- Always reading a token each clock, map to speed up
-- NEW REASONING - the no is wrong. Going to change this to just reading in
-- arrays of ints or bits, as that is what memory is. memory is a 1d array,
-- so these will just make parent array longer or wrap int/bit in array
attemptSpeedUp throughMult op@(MemRead (T_Array n t)) =
  (MemRead (T_Array (n*throughMult) t), throughMult)
attemptSpeedUp throughMult op@(MemRead t) =
  (MemRead (T_Array throughMult t), throughMult)
attemptSpeedUp throughMult op@(MemWrite (T_Array n t)) =
  (MemWrite (T_Array (n*throughMult) t), throughMult)
attemptSpeedUp throughMult op@(MemWrite t) =
  (MemWrite (T_Array throughMult t), throughMult)
-- NOTE: assuming that all pxPerClock are 1 unless inner dims pxPerClock ==
-- inner img dims
-- NOTE: for all dimensions, img dimension % pxPerClock == 0
-- NOTE: This works by speeding up inner dimensions before outer ones. Only
-- works if throughMult satisfies two conditions:
-- 1. Amount to make each full throuhgput dimension go from current pxPerClock
-- to full throughput divides cleanly into the throughputMult.
-- Stated rigorously: all i where i is number of full throuhgput dimensions:
--    ((\Pi_(0 to i-1) pxPerClock dim i) * throuhgMult) %
--     (\Pi_(0 to i-1) product of full throuhgput dims) == 0
-- 2. After making all inner dimensions full throuhgput, the first dimension not
-- made full throughput must consume the rest of throuhgputMult and result in a
-- new pxPerClock that cleanly divides into that dimension.
-- Stated rigorously:
--    (first not full throuhgput dim) % (throuhgMult) / (product of inner px
--     per clock increases) == 0
attemptSpeedUp throughMult (LineBuffer p w img t) =
  (LineBuffer (reverse reversedNewP) w img t, actualMult)
  where
    (reversedNewP, actualMult) =
      increaseLBPxPerClock (reverse p) (reverse img) throughMult
-- don't increase the arr size, duplicate it so it can be fed to other maps
attemptSpeedUp throughMult op@(Constant_Int _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Constant_Bit _) = (MapOp throughMult op, throughMult)
-- not going to change SLen in as, if 1, don't want to cause it to become
-- fractional
attemptSpeedUp throughMult (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) t) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn * throughMult)
    (sLenOut, oldArrLenOut * throughMult) t, throughMult)
attemptSpeedUp throughMult op@(ArrayReshape _ _) =
  (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(DuplicateOutputs _ _) =
  (MapOp throughMult op, throughMult)

-- is this an ok inverse of our decision to only underutil for slowdown of seq operators?
-- should this try to speed up children before itself?
attemptSpeedUp throughMult (MapOp par innerOp) | isComb innerOp =
  (MapOp (par*throughMult) innerOp, throughMult)
attemptSpeedUp throughMult (MapOp par innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp throughMult innerOp
  in (MapOp par spedUpInnerOp, actualMult)

-- only do it
-- 2. less than full parallel and numComb % newPar == 0 or
-- 3. more than parallel and newPar % numComb == 0
-- 4. everything inside is comb,
-- otherwise just speed up inside
-- NOTE: should I not be pushing down here? Should I give up if div doesn't work?
attemptSpeedUp throughMult (ReduceOp par numComb innerOp) | isComb innerOp &&
  (((newPar <= numComb) && ((numComb `mod` newPar) == 0)) ||
  ((newPar > numComb) && ((newPar `mod` numComb) == 0))) =
  (ReduceOp newPar numComb innerOp, throughMult)
  where newPar = par*throughMult
attemptSpeedUp throughMult (ReduceOp par numComb innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp throughMult innerOp
  in (ReduceOp par numComb spedUpInnerOp, actualMult)

attemptSpeedUp throughMult op@(NoOp _) = (MapOp throughMult op, throughMult)
-- this doesn't work when speeding up a linebuffer, the dkPairs value seems like
-- it needs to be reworked here.
attemptSpeedUp throughMult (Crop dkPairss cCps innerOp) =
  (Crop dkPairss cCps spedUpInnerOp, innerMult)
  where (spedUpInnerOp, innerMult) = attemptSpeedUp throughMult innerOp 
attemptSpeedUp throughMult (Delay dkPairs innerOp) =
  (Delay dkPairs spedUpInnerOp, innerMult)
  where (spedUpInnerOp, innerMult) = attemptSpeedUp throughMult innerOp 
-- modify underutil if mult divides cleanly into underutil's denominator
-- or if removing underutil and the denominator divides cleanly into mult
attemptSpeedUp throughMult (Underutil denom op) |
  (denom `mod` throughMult) == 0 =
  (Underutil (denom `ceilDiv` throughMult) op, throughMult)
attemptSpeedUp throughMult (Underutil denom op) |
  (throughMult `mod` denom) == 0 =
  (spedUpOp, denom*innerMult)
  where
    remainingMult = throughMult `ceilDiv` denom
    (spedUpOp, innerMult) = attemptSpeedUp remainingMult op 
attemptSpeedUp throughMult op@(Underutil denom innerOp) =
  (Underutil denom innerSpedUpOp, innerMult)
  where (innerSpedUpOp, innerMult) = attemptSpeedUp throughMult innerOp

-- NOTE: what to do if mapping over a reg delay? Nothing? its sequential but,
-- unlike other sequential things like reduce, linebuffer its cool to duplicate
attemptSpeedUp throughMult (RegRetime d innerOp) =
  (RegRetime d spedUpInnerOp, innerMult)
  where (spedUpInnerOp, innerMult) = attemptSpeedUp throughMult innerOp 

attemptSpeedUp throughMult (ComposePar ops) = 
  let
    spedUpOpsAndMults = map (attemptSpeedUp throughMult) ops
    spedUpOps = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  in (ComposePar spedUpOps, minimum actualMults)
attemptSpeedUp throughMult (ComposeSeq ops) = 
  let
    spedUpOpsAndMults = map (attemptSpeedUp throughMult) ops
    (hdSpedUpOps:tlSpedUpOps) = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  -- doing a fold here instead of just making another composeSeq to make sure all
  -- ports still match 
  in (foldl (|>>=|) hdSpedUpOps tlSpedUpOps, minimum actualMults)
attemptSpeedUp _ op@(ComposeFailure _ _) = (op, 1)


-- helper function for linebuffer attemptSpeedUp, goes through, increasing
-- parallelism of each component take an op and make it run x times faster
-- without wrapping it, where x is the second argument.
-- This may not be possible for some ops without wrapping them in a map
-- so the pair returns a best effort speed up op and the amount it was sped up
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
  (imgInner : pOuter, mult * multOuter)
  where
    -- since requiring pInner to always divide into imgInner, this is ok
    remainingMultForOuterDims = (mult * pInner) `ceilDiv` imgInner
    (pOuter, multOuter) = increaseLBPxPerClock pTl imgTl remainingMultForOuterDims
increaseLBPxPerClock p _ _ = (p, 1)

-- If its combinational, pretty much always just wrap it in underutil, as if later
-- slow down again, will just increase multiple on underutil, never see this again
attemptSlowDown :: Int -> Op -> (Op, Int)
attemptSlowDown throughDiv op@(Add _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Sub _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Mul _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Div _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Max _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Min _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Ashr _ _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Shl _ _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Abs _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Not _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(And _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Or  _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(XOr _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Eq = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Neq = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Lt = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Leq = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Gt = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@Geq = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(LUT _) = (MapOp throughDiv op, throughDiv)
-- should it be possible to speed this up? Should I always just speed up by
-- wrapping in an array instead of increasing a wrapping array length if it exists?
-- OLD REASONING - no, can't speed this up or slow it down as don't want to
-- change the meaning of the data it reads every clock.
-- Always reading a token each clock, map to speed up
-- NEW REASONING - the no is wrong. Going to change this to just reading in
-- arrays of ints or bits, as that is what memory is. memory is a 1d array,
-- so these will just make parent array longer or wrap int/bit in array
attemptSlowDown throughDiv op@(MemRead (T_Array n t)) |
  n `mod` throughDiv == 0 =
  (MemRead (T_Array (n `ceilDiv` throughDiv) t), throughDiv)
attemptSlowDown throughDiv op@(MemRead t) =
  (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(MemWrite (T_Array n t)) |
  n `mod` throughDiv == 0 =
  (MemWrite (T_Array (n `ceilDiv` throughDiv) t), throughDiv)
attemptSlowDown throughDiv op@(MemWrite t) =
  (Underutil throughDiv op, throughDiv)
-- NOTE: assuming that all pxPerClock are 1 unless inner dims pxPerClock ==
-- inner img dims
-- NOTE: for all dimensions, img dimension % pxPerClock == 0
-- NOTE: This works by slowing down outer dimensions before inner ones. Only
-- works if throughDiv satisfies two conditions:
-- 1. Amount to make each non-full throuhgput dimension go from current pxPerClock
-- to 1 divides cleanly into the throughputDiv.
-- Stated rigorously: all i where i is number of non-full throuhgput dimensions:
--    ((\Pi_(0 to i-1) pxPerClock dim i) * throuhgMult) %
--     (\Pi_(0 to i-1) product of non-full throuhgput dims) == 0
-- 2. After making all outer dimensions 1 px per clock, the first dimension not
-- made 1 px per clock must consume the rest of throuhgputMult and result in a
-- new pxPerClock that cleanly divides into that dimension.
-- Stated rigorously:
--    (first not 1 px per clock dim) % (throuhgMult / (product of px per clock
--     decreases to all outer dimensions)) == 0
attemptSlowDown throughDiv (LineBuffer p w img t) =
  (LineBuffer newP w img t, actualDiv)
  where
    (newP, actualDiv) = decreaseLBPxPerClock p img throughDiv
-- can't shrink this, have to underutil
attemptSlowDown throughDiv op@(Constant_Int _) =
  (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Constant_Bit _) =
  (Underutil throughDiv op, throughDiv)
-- not going to change SLen in consistency with speed up
-- can only slow down if divisible
attemptSlowDown throughDiv (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) t) |
  (oldArrLenIn `mod` throughDiv == 0) && (oldArrLenOut `mod` throughDiv == 0) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn `ceilDiv` throughDiv)
    (sLenOut, oldArrLenOut `ceilDiv` throughDiv) t, throughDiv)
attemptSlowDown throughDiv op@(SequenceArrayRepack _ _ _) = (op, 1)
attemptSlowDown throughDiv op@(ArrayReshape _ _) =
  (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(DuplicateOutputs _ _) =
  (Underutil throughDiv op, throughDiv)

-- NOTE: should I not be pushing down here? Should I give up if div doesn't work?
attemptSlowDown throughDiv (MapOp par innerOp) | isComb innerOp &&
  (par `mod` throughDiv == 0) =
  (MapOp (par `ceilDiv` throughDiv) innerOp, throughDiv)
attemptSlowDown throughDiv (MapOp par innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown throughDiv innerOp
  in (MapOp par slowedInnerOp, actualDiv)

-- only do it
-- 1. par divisible by throughDiv,
-- 2. less than full parallel and numComb % newPar == 0 or
-- 3. more than parallel and newPar % numComb == 0
-- 4. everything inside is comb,
-- otherwise just speed up inside
-- NOTE: should I not be pushing down here? Should I give up if div doesn't work?
attemptSlowDown throughDiv (ReduceOp par numComb innerOp) |
  isComb innerOp &&
  (par `mod` throughDiv == 0) &&
  (((newPar <= numComb) && (numComb `mod` newPar == 0)) ||
  ((newPar > numComb) && (newPar `mod` numComb == 0))) =
  (ReduceOp newPar numComb innerOp, throughDiv)
  where newPar = par `ceilDiv` throughDiv
attemptSlowDown throughDiv (ReduceOp par numComb innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown throughDiv innerOp
  in (ReduceOp par numComb slowedInnerOp, actualDiv)

attemptSlowDown throughDiv op@(NoOp _) = (MapOp throughDiv op, throughDiv)
-- this doesn't work when speeding up a linebuffer, the dkPairs value seems like
-- it needs to be reworked here.
attemptSlowDown throughDiv (Crop dkPairss cCps innerOp) =
  (Crop dkPairss cCps slowedInnerOp, innerDiv)
  where (slowedInnerOp, innerDiv) = attemptSlowDown throughDiv innerOp 
attemptSlowDown throughDiv (Delay dkPairs innerOp) =
  (Delay dkPairs slowedInnerOp, innerDiv)
  where (slowedInnerOp, innerDiv) = attemptSlowDown throughDiv innerOp 
attemptSlowDown throughDiv (Underutil denom op) =
  (Underutil (denom * throughDiv) op, throughDiv)

-- NOTE: what to do if mapping over a reg delay? Nothing? its sequential but,
-- unlike other sequential things like reduce, linebuffer its cool to duplicate
attemptSlowDown throughDiv (RegRetime d innerOp) =
  (RegRetime d slowedInnerOp, innerMult)
  where (slowedInnerOp, innerMult) = attemptSlowDown throughDiv innerOp 

attemptSlowDown throughDiv (ComposePar ops) = 
  let
    slowedOpsAndMults = map (attemptSlowDown throughDiv) ops
    slowedOps = map fst slowedOpsAndMults
    actualDivs = map snd slowedOpsAndMults
  in (ComposePar slowedOps, maximum actualDivs)
attemptSlowDown throughDiv (ComposeSeq ops) = 
  let
    slowedOpsAndMults = map (attemptSlowDown throughDiv) ops
    (hdSlowedOps:tlSlowedOps) = map fst slowedOpsAndMults
    actualDivs = map snd slowedOpsAndMults
  -- doing a fold here instead of just making another composeSeq to make sure all
  -- ports still match 
  in (foldl (|>>=|) hdSlowedOps tlSlowedOps, maximum actualDivs)
attemptSlowDown _ op@(ComposeFailure _ _) = (op, 1)

slowDown throughDiv op | actualDiv == throughDiv = spedUpOp
  where (spedUpOp, actualDiv) = attemptSlowDown throughDiv op
slowDown throughDiv op =
  ComposeFailure (BadThroughputMultiplier throughDiv actualDiv) (op, ComposeSeq [])
  where (spedUpOp, actualDiv) = attemptSlowDown throughDiv op

-- given a LB's pxPerClock, its image dimesions, and a divisor to slow down,
-- speed up the pxPerCLock from inner most to outer most.
-- Returns the new pxPerClock (in reverse order of LB) and the amount sped up
-- NOTE: pxPerClcok and imgDimensions come in same order as for LB
decreaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if no more dimensions to speed up, return remaining pxPerClock 
decreaseLBPxPerClock [] _ _ = ([], 1)
-- if not reducing to 1 out this dimension, pOuter / div must divide into imgOuter
-- and div must divide into pOuter.
-- no need to recurse further as done filling out dimensions
decreaseLBPxPerClock (pOuter:pTl) (imgOuter:imgTl) div |
  div < pOuter && (pOuter `mod` div == 0) &&
  ((imgOuter * div) `mod` pOuter == 0) = ((pOuter `ceilDiv` div) : pTl, div)
-- if reducing dim to 1 px per clock, pOuter must must divide into div  cleanly
decreaseLBPxPerClock (pOuter:pTl) (imgOuter:imgTl) div |
  div >= pOuter && (div `mod` pOuter == 0) = 
  (1 : pInner, div * divInner)
  where
    -- since requiring pOuter to always divide into imgOuter, this is ok
    remainingDivForInnerDims = div `ceilDiv` pOuter
    (pInner, divInner) = decreaseLBPxPerClock pTl imgTl remainingDivForInnerDims
decreaseLBPxPerClock p _ _ = (p, 1)
