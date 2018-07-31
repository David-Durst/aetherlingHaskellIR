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
-- that doesn't match the divisibility requirements such as those for linebuffer.
-- Not going to speed these up by wrapping them in a map because that would
-- cause inappropriate behavior in the linebuffer. The linebuffer has complex
-- internal state and mapping over it will cause it to behave unintuitively.
-- For example, an LineBuffer [2] [3] [300] T_Int will reading in 2 int per clock
-- and emit two overlapping windows of consecutive ints.
-- A Map 2 (LineBuffer [1] [3] [300] T_Int) will emit two windows, but the
-- ints in the windows will not be consecutive. The first window will have ints
-- that came in 0, 2, and 4 (0-indexed) and the second window will have ints
-- indexed 1, 3, and 5.

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

-- If child op has internal state, can't automatically speed up child op by
-- changing par and making more copies. Making two independent copies with
-- different state won't behave same as modifying op to update its state to run
-- twice as fast
attemptSpeedUp throughMult (MapOp par innerOp) | not $ hasInternalState innerOp =
  (MapOp (par*throughMult) innerOp, throughMult)
attemptSpeedUp throughMult (MapOp par innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp throughMult innerOp
  in (MapOp par spedUpInnerOp, actualMult)

-- speed up reduce based on three cases:
-- 1. child op has internal state: just speed up child op.
-- Due to state, can't make multiple copies of child op for same reason as map
-- 2. child op has no internal state and throughMult has value so that
-- afterwards numComb >= newPar: increase parallelism factor to desired amount
-- ASSUMPTION: newPar must cleanly divide into numComb, or numComb % newPar == 0
-- 3. child op has no internal state and throughMult has value so that afterwards
-- numComb < newPar: make par == numComb and map over reduce to get rest
-- of parallelism
-- ASSUMPTION: numComb must cleanly divide into newPar, or
-- newPar % numComb == 0
-- 4. Child has no internal state, but the other conditions don't hold: speed up
-- the child
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

-- This operates by speeding up the inner most dimension first. Once a dimension
-- is fully parallel, then it speeds up the outer dimensions.

-- NOTE: Unlike for a LB, here the pxPerClock and img dimensions must have the
-- inner most dimension come first.
-- This is the reverse of what it is on the linebuffer

-- the following assumptions must hold before and after running speed up:
-- ASSUMPTION 1: all pxPerClock are 1 unless all more inner dimensions
-- are already fully parallelized
-- ASSUMPTION 2: for all dimensions, the parallelism cleanly divides into the
-- size of that dimension, or img dimension % pxPerClock == 0
-- In order for assumption 2 to hold, the requested mulitplier must make two
-- conditions true:
-- CONDITION 1: must be able to factor the request multiplier so that
-- a different factor makes each of the different fully parallelized dimensions
-- go from its pxPerClock prior to speed up to one that equals the image size
-- for that dimension after that speed up.
-- Stated rigorously: Let D be the set of dimensions that are fully parallel 
-- post slow down 
--    ((\Pi_(d \in D) pxPerClock dim d) * requestedMult) %
--     (\Pi_(d \in D) imgSize dim d) == 0
-- CONDITION 2. After making all more inner dimensions full throuhgput, the first 
-- dimension not made fully parallel must consume the rest of requested multiplier
-- and result in a new pxPerClock that cleanly divides into that dimension's
-- size
-- Stated rigorously:
--    (size of first not full throuhgput dim) % (requestMult /
--      (product of inner px per clock throughput increases)) == 0

increaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if mult is down to 1 or no more dimensions to speed up, return remaining pxPerClock 
increaseLBPxPerClock [] _ _ = ([], 1)
increaseLBPxPerClock p img requestedMult | requestedMult == 1 = (p, 1)
-- if not making this dimension fully parallel, new amount of parallelism for
-- the dimension (pInner * mult) must divide into the dimension's size
-- (imgInner). No need to recurse further as used up all of requested multiplier
-- for speeding up
increaseLBPxPerClock (pInner:pTl) (imgInner:imgTl) requestedMult |
  imgInner > pInner * requestedMult &&
  (imgInner `mod` (pInner * requestedMult) == 0) =
  ((pInner * requestedMult) : pTl, requestedMult)
-- if making this dimension fully parallel, imgInner must divide into
-- requestedMult * pInner cleanly so you can make the new parallelism for this
-- dimension equal to imgInner and then recur on speeding up the outer
-- dimensions with the remaining part of requestedMult
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

-- This is the helper function that slows down an op as much as possible
-- and returns the amount slowed down.

-- LEAF, NON-MODIFIABLE RATE 
-- can't change rate, can't slow child ops, so just wrap in an underutil to
-- slow down. This is the same approach as speed up, but with underutil instead
-- of map, with same assumption regarding not changing the type.
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

-- underutil instead of changing type for same reason as speedUp using map,
-- want to do banking instead of making wider memories
attemptSlowDown throughDiv op@(MemRead _) = (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(MemWrite _) = (Underutil throughDiv op, throughDiv)

attemptSlowDown throughDiv op@(ArrayReshape _ _) =
  (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(DuplicateOutputs _ _) =
  (Underutil throughDiv op, throughDiv)

attemptSlowDown throughDiv op@(Constant_Int _) =
  (Underutil throughDiv op, throughDiv)
attemptSlowDown throughDiv op@(Constant_Bit _) =
  (Underutil throughDiv op, throughDiv)

-- LEAF, MODIFIABLE RATE
-- If possible, slow down by changing the rate. Otherwise, just try to return
-- an op with the same or slower throughput than the original.
-- Behavior undefined in same case as for attemptSpeedUp.
-- Not worth supporting slowdown using underutil in event of being unable
-- to adjust rate as can do symmetric behavior for speed up. While you
-- can wrap underutil around a stateful operator like a linebuffer, you can't
-- wrap a map around it for the reasons discussed in the attemptSpeedUp comments.
-- Thus, to keep the symmetry, not going to underutil linebuffers.

-- Slow down outer most dimension first, then slow down more inner dimensions.
-- see decreaseLBPxPerClock for more information
-- NOTE: decreaseLBPxPerClock expects dimensions from outermost to innermost
-- which matches the order for LineBuffer.
attemptSlowDown throughDiv (LineBuffer p w img t bc) =
  (LineBuffer newP w img t bc, actualDiv)
  where
    (newP, actualDiv) = decreaseLBPxPerClock p img throughDiv

-- not going to change SLen in consistency with speed up
-- can only slow down if both array lengths are divisible by throughDiv
attemptSlowDown throughDiv (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) t) |
  (oldArrLenIn `mod` throughDiv == 0) && (oldArrLenOut `mod` throughDiv == 0) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn `ceilDiv` throughDiv)
    (sLenOut, oldArrLenOut `ceilDiv` throughDiv) t, throughDiv)
attemptSlowDown throughDiv op@(SequenceArrayRepack _ _ _) = (op, 1)

-- PARENT, NON-MODIFIABLE RATE
-- Slow their child ops, no rate to modify on these, and no
-- point in underutiling these as can just defer that to children.

attemptSlowDown throughDiv op@(NoOp _) = (MapOp throughDiv op, throughDiv)
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

-- PARENT, MODIFIABLE RATE
-- Slow the parent by decreasing the rate if possible. If not possible,
-- try to slow the children.
-- The default strategy is to adjust the rate, then fall back to slowing
-- the children if the rate can't be adjusted

-- If child has internal state, can't automatically slow down child as changing
-- number of child ops is different from running each one at a lower rate when
-- each child op is managing state. This is the same reasoning as speed up.
attemptSlowDown throughDiv (MapOp par innerOp) | not $ hasInternalState innerOp &&
  (par `mod` throughDiv == 0) =
  (MapOp (par `ceilDiv` throughDiv) innerOp, throughDiv)
attemptSlowDown throughDiv (MapOp par innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown throughDiv innerOp
  in (MapOp par slowedInnerOp, actualDiv)

-- slow down reduce based on three cases:
-- 1. child op has internal state: just slow down child op
-- Due to state, can't change number of copies of child op for same reason as map
-- 2. child op has no internal state: decrease parallelism factor to desired
-- amount
-- ASSUMPTION: throughDiv must cleanly divide into Par, or par % throughDiv == 0
-- ASSUMPTION: newPar must cleanly divide into numComb, or numComb % newPar == 0
-- 3. child op has no internal state but other conditions don't hold: slow down
-- the child op
attemptSlowDown throughDiv (ReduceOp par numComb innerOp) |
  (not $ hasInternalState innerOp) &&
  (par `mod` throughDiv == 0) &&
  (numComb `mod` newPar == 0) =
  (ReduceOp newPar numComb innerOp, throughDiv)
  where newPar = par `ceilDiv` throughDiv
attemptSlowDown throughDiv (ReduceOp par numComb innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown throughDiv innerOp
  in (ReduceOp par numComb slowedInnerOp, actualDiv)

attemptSlowDown throughDiv (Underutil denom op) =
  (Underutil (denom * throughDiv) op, throughDiv)

attemptSlowDown _ op@(Failure _) = (op, 1)

-- helper that actually implements linebuffer's attemptSlowDown 
-- given a LB's pxPerClock, its image dimesions, and a multiple to slow down,
-- speed up the pxPerCLock from inner most to outer most.
-- Returns the new pxPerClock (in the same order as LB) and the amount slowed

-- This operates by slowing down the outer most dimension first. Once that
-- dimension is fully sequential, then it slows down more inner dimensions.

-- the following assumptions must hold before and after running speed up:
-- NOTE: these are the same assumptions as for increaseLBPxPerClock
-- ASSUMPTION 1: all pxPerClock are 1 unless all more inner dimensions
-- are already fully parallelized
-- ASSUMPTION 2: for all dimensions, the parallelism cleanly divides into the
-- size of that dimension, or img dimension % pxPerClock == 0
-- In order for assumption 2 to hold, the requested divisor must make two
-- conditions true:
-- CONDITION 1: must be able to factor the requested divisor so that
-- a different factor makes each of the different fully sequential dimensions
-- go from its pxPerClock prior to speed up to 1 after the slow down
-- Stated rigorously: Let D be the set of dimensions that are fully sequential
-- post slow down 
--    requestedDiv % (\Pi_(d \in D) pxPerClock for dimension d pre slowdown) == 0
-- 2. After making all outer dimensions 1 px per clock, the first dimension not
-- made 1 px per clock must consume the rest of throuhgputMult and result in a
-- new pxPerClock that cleanly divides into that dimension.
-- Stated rigorously:
--    (first not 1 px per clock dim) % (throuhgMult / (product of px per clock
--     decreases to all outer dimensions)) == 0
-- given a LB's pxPerClock, its image dimesions, and a divisor to slow down,
-- speed up the pxPerCLock from inner most to outer most.
-- Returns the new pxPerClock (in reverse order of LB) and the amount sped up
-- NOTE: pxPerClcok and imgDimensions come in same order as for LB
decreaseLBPxPerClock :: [Int] -> [Int] -> Int -> ([Int], Int)
-- if no more dimensions to slow, return remaining pxPerClock 
decreaseLBPxPerClock [] _ _ = ([], 1)
-- if not making this dimension fully sequential, new amount of parallelism post
-- slow down (pOuter / div) must divide into the dimension's size (imgOuter).
-- Also, div must divide into pOuter for this to work.
-- No need to recurse further as used up all of requested div 
-- for slowing down. 
decreaseLBPxPerClock (pOuter:pTl) (imgOuter:imgTl) requestedDiv |
  requestedDiv < pOuter && (pOuter `mod` requestedDiv == 0) &&
  ((imgOuter * requestedDiv) `mod` pOuter == 0) =
  ((pOuter `ceilDiv` requestedDiv) : pTl, requestedDiv)
-- if making this dimension fully sequential, the pre-slow down parallelism
-- (pOuter) must divide cleanly into the requested div
decreaseLBPxPerClock (pOuter:pTl) (imgOuter:imgTl) requestedDiv |
  requestedDiv >= pOuter && (requestedDiv `mod` pOuter == 0) = 
  (1 : pInner, requestedDiv * divInner)
  where
    -- since requiring pOuter to always divide into imgOuter, this is ok
    remainingDivForInnerDims = requestedDiv `ceilDiv` pOuter
    (pInner, divInner) = decreaseLBPxPerClock pTl imgTl remainingDivForInnerDims
decreaseLBPxPerClock p _ _ = (p, 1)
