module STThroughputPasses where
import STTypes
import STAST
import STMetrics
import STAnalysis

-- given a LB's pxPerClock, its image dimesions, and a multiple to speed up, speed up the pxPerCLock from
-- inner most to outer most.
-- Returns the new pxPerClock (in reverse order of LB) and the amount sped up
-- NOTE: Unlike for a LB, here the pxPerClock and img dimensions must have the inner most dimension come first
-- this is the reverse of what it is on the linebuffer
increaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if mult is down to 1 or no more dimensions to speed up, return remaining pxPerClock 
increaseLBPxPerClock [] _ _ = ([], 1)
increaseLBPxPerClock p img mult | mult == 1 = (p, 1)
-- if not filling out this dimension, pInner * mult must divide into imgInner
-- no need to recurse further as done filling out dimensions
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner > pInner * mult && (imgInner `mod` (pInner * mult) == 0) = ((pInner * mult) : pTl, mult)
-- if filling out this dimension, imgInner must divide into mult * pInner cleanly
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner <= pInner * mult && ((mult * pInner) `mod` imgInner == 0) = (imgInner : pTl, mult * multOuter)
  where
    -- since requiring pInner to always divide into imgInner, this is ok
    remainingMultForOuterDims = mult `ceilDiv` (imgInner `ceilDiv` pInner)
    (pOuter, multOuter) = increaseLBPxPerClock pTl imgTl remainingMultForOuterDims
increaseLBPxPerClock p _ _ = (p, 1)

-- helper function for linebuffer speedUpIfPossible, goes through, increasing parallelism of each component
-- take an op and make it run x times faster without wrapping it, where x is the second argument
-- This may not be possible for some ops without wrapping them in a map
-- so the pair returns a best effort speed up op and the amount it was sped up
speedUpIfPossible :: Op -> Int -> (Op, Int)
speedUpIfPossible op@(Add _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Sub _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Mul _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Div _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Max _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Min _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Ashr _ _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Shl _ _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Abs _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Not _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(And _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(Or  _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@(XOr _) throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Eq throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Neq throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Lt throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Leq throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Gt throughMult = (MapOp throughMult op, throughMult)
speedUpIfPossible op@Geq throughMult = (MapOp throughMult op, throughMult)
-- should it be possible to speed this up? Should I always just speed up by wrapping in an array
-- instead of increasing a wrapping array length if it exists?
-- OLD REASONING - no, can't speed this up or slow it down as don't want to change the meaning of the data it
-- reads every clock. Always reading a token each clock, map to speed up
-- NEW REASONING - the no is wrong. Going to change this to just reading in arrays of ints or bits, as that is what memory is.
-- memory is a 1d array, so these will just make parent array longer or wrap int/bit in array
speedUpIfPossible op@(MemRead (T_Array n t)) throughMult = (MemRead (T_Array (n*throughMult) t), throughMult)
speedUpIfPossible op@(MemRead t) throughMult = (MemRead (T_Array throughMult t), throughMult)
speedUpIfPossible op@(MemWrite (T_Array n t)) throughMult = (MemWrite (T_Array (n*throughMult) t), throughMult)
speedUpIfPossible op@(MemWrite t) throughMult = (MemWrite (T_Array throughMult t), throughMult)
-- NOTE: assuming that all pxPerClock are 1 unless inner dims pxPerClock == inner img dims
-- NOTE: for all dimensions, img dimension % pxPerClock == 0
-- NOTE: This works by speeding up inner dimensions before outer ones. Only works if throughMult satisfies two conditions:
-- 1. Amount to make each full throuhgput dimension go from current pxPerClock to full throughput divides cleanly into the throughputMult
-- Stated rigorously: all i where i is number of full throuhgput dimensions:
--    ((\Pi_(0 to i-1) pxPerClock dim i) * throuhgMult) % (\Pi_(0 to i-1) product of full throuhgput dims) == 0
-- 2. After making all inner dimensions full throuhgput, the first dimension not made full throughput must
-- consume the rest of throuhgputMult and result in a new pxPerClock that cleanly divides into that dimension.
-- Stated rigorously:
--    (first not full throuhgput dim) % ((product of all pxPerClock * throuhgMult) / (product of full throuhgput dims)) == 0
speedUpIfPossible (LineBuffer p w img t) throughMult = (LineBuffer (reverse reversedNewP) w img t, actualMult)
  where
    (reversedNewP, actualMult) = increaseLBPxPerClock (reverse p) (reverse img) throughMult
speedUpIfPossible (Constant_Int constArr) throughMult = (Constant_Int $ foldl (++) [] $
                                                         replicate throughMult constArr, throughMult)
speedUpIfPossible (Constant_Bit constArr) throughMult = (Constant_Bit $ foldl (++) [] $
                                                         replicate throughMult constArr, throughMult)


-- if going to speed 
