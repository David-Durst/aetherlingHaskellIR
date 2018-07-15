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
increaseLBPxPerClock :: [Int] -> [Int] -> Int -> [Int]
-- if mult is down to 1 or no more dimensions to speed up, return remaining pxPerClock 
increaseLBPxPerClock [] _ _ = ([], 1)
increaseLBPxPerClock p img mult | mult == 1 = (p, 1)
-- if can still speed this up in this dimension, do so
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult = (newPxPerClockThisDim : pOuter, multThisDim * multOuter)
  where
    -- imgInner must always be a multiple of pInner, so the result of this must be a multiple of pInner
    newPxPerClockThisDim = gcd imgInner (pInner * mult)
    -- this must be <= mult as newPxPerClockThisDim can increase by at most mult relative to pInner
    -- rounding not an issue here as newPxPerClockThisDim multiple of pInner
    multThisDim = newPxPerClockThisDim `ceilDiv` pInner
    -- if can't cleanly divide multThisDim into mult, then stop as just going to make more of a mess
    remainingMultForOuterDims = if mult `mod` multThisDim == 0
      then mult `ceilDiv` multThisDim
      else 1
    -- if filled out this dimension, go onto next, otherwise stop
    (pOuter, multOuter) = if imgInner == newPxPerClockThisDim
      then increaseLBPxPerClock pTl imgTl remainingMultForOuterDims
      else (pTl, 1)

-- if not filling out this dimension, pInner * mult must divide into imgInner
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner > pInner * mult && (imgInner `mod` (pInner * mult) == 0) = ((pInner * mult) : pTl, mult)
-- if filling out this dimension, imgInner must divide into mult
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) mult |
  imgInner <= pInner * mult && (mult `mod` imgInner == 0) = ((pInner * mult) : pTl, mult)
  where
    remainingMultForOuterDims = mult `ceilDiv` multThisDim
    (pOuter, multOuter) = increaseLBPxPerClock pTl imgTl remainingMultForOuterDims

-- helper function for linebuffer speedUpIfPossible, goes through, increasing parallelism of each component
-- take an op and make it run x times faster without wrapping it, where x is the second argument
-- This may not be possible for some ops without wrapping them in a map
-- so the pair returns a best effort speed up op and the amount it was sped up
speedUpIfPossible :: Op -> Int -> (Op, Int)
speedUpIfPossible op@(Add _) throughMult = (op, 1)
speedUpIfPossible op@(Sub _) throughMult = (op, 1)
speedUpIfPossible op@(Mul _) throughMult = (op, 1)
speedUpIfPossible op@(Div _) throughMult = (op, 1)
speedUpIfPossible op@(Max _) throughMult = (op, 1)
speedUpIfPossible op@(Min _) throughMult = (op, 1)
speedUpIfPossible op@(Ashr _ _) throughMult = (op, 1)
speedUpIfPossible op@(Shl _ _) throughMult = (op, 1)
speedUpIfPossible op@(Abs _) throughMult = (op, 1)
speedUpIfPossible op@(Not _) throughMult = (op, 1)
speedUpIfPossible op@(And _) throughMult = (op, 1)
speedUpIfPossible op@(Or  _) throughMult = (op, 1)
speedUpIfPossible op@(XOr _) throughMult = (op, 1)
speedUpIfPossible op@Eq throughMult = (op, 1)
speedUpIfPossible op@Neq throughMult = (op, 1)
speedUpIfPossible op@Lt throughMult = (op, 1)
speedUpIfPossible op@Leq throughMult = (op, 1)
speedUpIfPossible op@Gt throughMult = (op, 1)
speedUpIfPossible op@Geq throughMult = (op, 1)
-- should it be possible to speed this up? Should I always just speed up by wrapping in an array
-- instead of increasing a wrapping array length if it exists?
-- no, can't speed this up or slow it down as don't want to change the meaning of the data it
-- reads every clock. Always reading a token each clock, map to speed up
speedUpIfPossible op@(MemRead _) throughMult = (op, 1) 
speedUpIfPossible op@(MemRead _) throughMult = (op, 1)
-- NOTE: assuming that all pxPerClock are 1 unless inner dims pxPerClock == inner img dims
-- NOTE: If not choosing a multiple such that the (product of all pxPerClock * throuhgMult) % (product of full throuhgput dims) == 0 and
-- (first not full throuhgput dim) % ((product of all pxPerClock * throuhgMult) / (product of full throuhgput dims)) == 0, then no guarantees
-- this works
speedUpIfPossible op@(LineBuffer p w _ _) throughMult = (op, 1)
  where
    -- from inner most to outer most, speed up and move on to next when saturate whole dimension
    newP = foldr (\(oldPEl, wEl) (newPList, throughMultRemaining) ->
                    speedUpAndRemainder oldPEl wEl throughMultRemaining )
    -- speed up one dimesnion as much as possible
    speedUpOneDim oldPEl wEl throughMultRemaining = throughMultRemaining `ceilDiv` (wEl `ceilDiv` pEl)
    -- get one dim sped up as much as possible and the remainig amount of speedup
    speedUpAndRemainder oldPEl wEl throughMultRemaining =
      (speedUpOneDim oldPEl wEl throughMultRemaining, throughMultRemaining `ceilDiv` speedUpOneDim oldPEl wEl throughMultRemaining
