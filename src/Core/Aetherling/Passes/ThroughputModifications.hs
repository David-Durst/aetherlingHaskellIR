{-|
Module: Aetherling.Passes.ThroughputModifications
Description: Passes that tradeoff throughput and area

The Aetherling Operations. These are split into four groups:

1. Leaf, non-modifiable rate - these are arithmetic, boolean logic,
and bit operations that don't contain any other ops and don't have
a parameter for making them run with a larger or smaller throughput.
Since these don't have a modifiable rate, they are sped up and slowed
down by wrapping them in a parent, modifiable rate op such as map and
underutil.

2. Leaf, modifiable rate - these are ops like linebuffers,
and space-time type reshapers that have a parameter for changing their
throughput and typically are not mapped over to change their throuhgput.
These ops don't have child ops.
Since these have a modifiable rate, they are sped up and slowed down by
trying to adjust that rate. In some cases, it may not be possible to adjust
the rate if the op with the new is invalid given the dimensions of the data
being operated on. Speed up and slow down may fail in these cases.

3. Parent, non-modifiable rate - these ops like composeSeq and composePar have
child ops that can have their throughputs' modified, but the parent
op doesn't have a parameter that affects throughput

4. Parent, modifiable rate - map is the canonical example. It has child ops
and can have its throughput modified by changing parallelism.

The four groups are have their throughputs increased and decreased using
different approaches:

1. Leaf, non-modifiable rate - these ops are sped up and slowed down by wrapping
them in a parent, modifiable rate op such as map and underutil. 

2. Leaf, modifiable rate - these ops are sped up and slowed down by trying to
adjust their rate. In some cases, it may not be possible to adjust the rate if
the op with the new is invalid given the dimensions of the data being operated
on. Speed up and slow down may fail in these cases.

3. Parent, non-modifiable rate - these ops are sped up and slowed by down
adjusting the throughputs of their child ops. 

4. Parent, modifiable rate - these ops are sped up and slowed down by first
trying to adjust their rate. If that is not possible, speedUp and slowDown
try to adjust the throughputs of their child ops.
-}
module Aetherling.Passes.ThroughputModifications (speedUp, slowDown) where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Operations.Compose
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics

-- NOTE: AM I PUSHING TOO MUCH LOGIC INTO THE LEAVES BY HAVING THEM
-- UNDERUTIL THEMSELVES? SHOULD THE COMPOSESEQs DO UNDERUTIL WHEN CHILDREN CAN'T?

-- | Increase the throughput of an Aetherling DAG by increasing area and
-- utilization. speedUp attempts to increase throughput by increasing
-- utilization before using more area.
speedUp throughMult op | actualMult == throughMult = spedUpOp
  where (spedUpOp, actualMult) = attemptSpeedUp throughMult op
speedUp throughMult op = Failure
  (InvalidThroughputModification throughMult actualMult) 
  where (spedUpOp, actualMult) = attemptSpeedUp throughMult op

-- This is the helper function that speeds up an op as much as possible
-- and returns the amount sped up.
-- it is split up into the four types of ops described at the top of the file.

attemptSpeedUp :: Int -> Op -> (Op, Int)
-- LEAF, NON-MODIFIABLE RATE 
-- can't change rate, can't speed up child ops, so just wrap in a map to
-- parallelize
-- ASSUMPTION: the user has specified a type for these ops and passes will not
-- change the type because that would change the semantics of the
-- program. For example, one could speed up an Add T_Int my making it a
-- Add $ T_Array 2 T_Int, but that would change the program's meaning.
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

-- Must map over these because making a single memory wider is a more
-- expensive operation than banking it (making multiple copies)
-- NOTE: unclear what expensive means here, just my understanding from
-- talking with Spatial team, Ross, and other hardware people. I should
-- figure it out.
-- maping over a MemRead/MemWrite is banking it, and wrapping
-- the type with an array is making a single memory read/write more
-- per clock.
attemptSpeedUp throughMult op@(MemRead _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(MemWrite _) = (MapOp throughMult op, throughMult)

-- These are leaf, non-modifiable rate unlike SequenceArrayRepack because,
-- for these operations, the user has not specified the type separately from the
-- array length. Therefore, speedup may modify the meaning of the program
-- by changing the types of these ops.
attemptSpeedUp throughMult op@(ArrayReshape _ _) =
  (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(DuplicateOutputs _ _) =
  (MapOp throughMult op, throughMult)

attemptSpeedUp throughMult op@(Constant_Int _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult op@(Constant_Bit _) = (MapOp throughMult op, throughMult)

-- LEAF, MODIFIABLE RATE
-- If possible, speed up by changing the rate. Otherwise, just try to return
-- an op with the same or greater throughput than the original.
-- However, behavior is undefined if requesting an invalid throughput multiplier
-- Not going to speed these up by wrapping them in a map because that would
-- cause inappropriate behavior in the linebuffer. The linebuffer has complex
-- internal state and mapping over it will cause it to behave unintuitively.
-- For example, an LineBuffer [2] [3] [300] T_Int will reading in 2 int per clock
-- and emit two overlapping windows of consecutive ints.
-- A Map 2 (LineBuffer [1] [3] [300] T_Int) will emit two windows, but the
-- ints in the windows will not be consecutive. The first window will have ints
-- that came in 0, 2, and 4 (0-indexed) and the second window will have ints
-- indexed 1, 3, and 5.

-- For linebuffer:
-- speed up inner most dimension first. Once you have an entire row input per
-- clock, then speed up the outer dimensions like number of rows per clock.
-- see increaseLBPxPerClock for more information. 
-- NOTE: increaseLBPxPerClock expects dimensions from innermost to outermost
-- and LineBuffer stores them in the opposite order. Thus, they must be
-- reversed when calling the helper function.
attemptSpeedUp throughMult (LineBuffer p w img t bc) =
  (LineBuffer (reverse reversedNewP) w img t bc, actualMult)
  where
    (reversedNewP, actualMult) =
      increaseLBPxPerClock (reverse p) (reverse img) throughMult

-- could decrease sLenIn and sLenOut as increase throughput, but not going
-- to do that as don't want to deal with fractional sequence lengths,
-- which could happen if dividing sLenIn or sLenOut 
attemptSpeedUp throughMult (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) t) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn * throughMult)
    (sLenOut, oldArrLenOut * throughMult) t, throughMult)


-- PARENT, NON-MODIFIABLE RATE
-- Speed up their child ops, no rate to modify on these, and no
-- point in mapping over these as can just defer that to doing over children.

attemptSpeedUp throughMult op@(NoOp _) = (MapOp throughMult op, throughMult)
attemptSpeedUp throughMult (Delay d innerOp) =
  (Delay d spedUpInnerOp, innerMult)
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

-- PARENT, MODIFIABLE RATE 
-- speed up the parent by increasing the rate if possible. If not possible,
-- try to speed up the children.
-- The default strategy is to adjust the rate, then fallback to speeding up
-- the children if the rate can't be adjusted.

attemptSpeedUp throughMult (MapOp par innerOp) | not $ hasInternalState innerOp =
  (MapOp (par*throughMult) innerOp, throughMult)
attemptSpeedUp throughMult (MapOp par innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp throughMult innerOp
  in (MapOp par spedUpInnerOp, actualMult)

-- speed up reduce using four approaches:
-- 1. child op has internal state: just speed up child op.
-- Due to state, can't automatically speed up child op by making more
-- copies as making two independent copies with different state stores
-- won't behave same as modifying one and to update its state to run
-- twice as fast
-- 2. child op has no internal state and speed up so that
-- afterwards numComb >= newPar: increase parallelism factor to desired amount
-- ASSUMPTION: newPar must cleanly divide into numComb, or numComb % newPar == 0
-- 3. child op has no internal state and speed up so that afterwards
-- numComb < newPar: make par == numComb and map over reduce to get rest
-- of parallelism
-- ASSUMPTION: numComb must cleanly divide into newPar, or
-- newPar % numComb == 0
attemptSpeedUp throughMult (ReduceOp par numComb innerOp) |
  (not $ hasInternalState innerOp) &&
  ((newPar <= numComb) && ((numComb `mod` newPar) == 0)) =
  (ReduceOp newPar numComb innerOp, throughMult)
  where newPar = par*throughMult
attemptSpeedUp throughMult (ReduceOp par numComb innerOp) |
  (not $ hasInternalState innerOp) &&
  ((newPar > numComb) && ((newPar `mod` numComb) == 0)) =
  (MapOp mapPar $ ReduceOp numComb numComb innerOp, throughMult)
  where
    newPar = par*throughMult
    mapPar = throughMult `ceilDiv` (numComb `ceilDiv` par)
attemptSpeedUp throughMult (ReduceOp par numComb innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp throughMult innerOp
  in (ReduceOp par numComb spedUpInnerOp, actualMult)

-- cases:
-- 1. if throughMult less than denom and throughMult divides cleanly
-- into denom, just decrease the underutil denom
-- 2. if throughMult greater than denom and denom divides cleanly into
-- throughMult, remove the underutil and speed up the innerOp using the
-- remaining part of throughMult
-- 3. fall back, just speed up the inner op
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

attemptSpeedUp _ op@(Failure _) = (op, 1)


-- helper that actually implements linebuffer's attemptSpeedUp 
-- given a LB's pxPerClock, its image dimesions, and a multiple to speed up,
-- speed up the pxPerCLock from inner most to outer most.
-- Returns the new pxPerClock (in reverse order of LB) and the amount sped up

-- NOTE: Unlike for a LB, here the pxPerClock and img dimensions must have the
-- inner most dimension come first.
-- This is the reverse of what it is on the linebuffer

-- the following assumptions must hold before and after running speed up:
-- ASSUMPTION 1: all pxPerClock are 1 unless all more inner dimensions
-- are already fully parallelized
-- ASSUMPTION 2: for all dimensions, the parallelism cleanly divides into the
-- size of that dimension, or img dimension % pxPerClock == 0
-- In order for assumption 2 to hold, the request mulitplier must make two
-- conditions true:
-- CONDITION 1: must be able to factor the request multiplier so that
-- a different factor makes each of the different fully parallelized dimensions
-- go from its pxPerClock prior to speed up to one that equals the image size
-- for that dimension after that speed up.
-- Stated rigorously: all i where i is number of full throuhgput dimensions:
--    ((\Pi_(0 to i-1) pxPerClock dim i) * requestedMult) %
--     (\Pi_(0 to i-1) product of full throuhgput dims) == 0
-- 2. After making all more inner dimensions full throuhgput, the first 
-- dimension not made fully parallel must consume the rest of requested multiplier
-- and result in a new pxPerClock that cleanly divides into that dimension's
-- size
-- Stated rigorously:
--    (first not full throuhgput dim) % (requestMult / (product of inner px
--     per clock throughput increases)) == 0

increaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if mult is down to 1 or no more dimensions to speed up, return remaining pxPerClock 
increaseLBPxPerClock [] _ _ = ([], 1)
increaseLBPxPerClock p img requestedMult | requestedMult == 1 = (p, 1)
-- if not filling out this dimension, pInner * mult must divide into imgInner
-- no need to recurse further as done filling out dimensions
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) requestedMult |
  imgInner > pInner * requestedMult &&
  (imgInner `mod` (pInner * requestedMult) == 0) =
  ((pInner * requestedMult) : pTl, requestedMult)
-- if filling out this dimension, imgInner must divide into mult * pInner cleanly
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) requestedMult |
  imgInner <= pInner * requestedMult &&
  ((requestedMult * pInner) `mod` imgInner == 0) =
  (imgInner : pOuter, requestedMult * multOuter)
  where
    -- since requiring pInner to always divide into imgInner, this is ok
    remainingMultForOuterDims = (requestedMult * pInner) `ceilDiv` imgInner
    (pOuter, multOuter) = increaseLBPxPerClock pTl imgTl remainingMultForOuterDims
increaseLBPxPerClock p _ _ = (p, 1)

-- | Decrease the throughput an Aetherling DAG by decreasing area and
-- utilization. slowDown attempts to decrease throughput by decreasing area
-- before underutilizing.
slowDown throughDiv op | actualDiv == throughDiv = spedUpOp
  where (spedUpOp, actualDiv) = attemptSlowDown throughDiv op
slowDown throughDiv op = Failure $ InvalidThroughputModification throughDiv actualDiv
  where (spedUpOp, actualDiv) = attemptSlowDown throughDiv op

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
attemptSlowDown throughDiv (LineBuffer p w img t bc) =
  (LineBuffer newP w img t bc, actualDiv)
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
attemptSlowDown throughDiv (Underutil denom op) =
  (Underutil (denom * throughDiv) op, throughDiv)

-- NOTE: what to do if mapping over a reg delay? Nothing? its sequential but,
-- unlike other sequential things like reduce, linebuffer its cool to duplicate
attemptSlowDown throughDiv (Delay d innerOp) =
  (Delay d slowedInnerOp, innerMult)
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
attemptSlowDown _ op@(Failure _) = (op, 1)

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
