{-|
Module: Aetherling.Passes.ThroughputModifications
Description: Passes that tradeoff throughput and area

The two main passes that modify the throughput of Aetherling operations are
speedup and slowdown. These increase and decrease the throughput of the
operations while preserving the order and values of inputs and outputs.

For the purposes of speedup and slowdown, Aetherling ops are split using two
sets of categories:

1. Parent or Leaf - An op is a parent op if it contains child ops. An op is
a leaf op if it does not contain child ops. This is the same terminology as
all other tree data structures.

2. Directly or indirectly scalable - An op is directly scalable if it has a
parameter for directly scaling its throughput. An op is indirectly scalable
if it does not have such a parameter. Indirectly scalable ops are scaled
either by wrapping them in other ops or by scaling their child ops.

These categories form four groups:

1. Leaf, indirectly scalable - These ops both do not contain any child ops and
also do not have a parameter for directly scaling their throughput. Examples of
these ops are arithmetic, boolean logic, and bit operations.

2. Leaf, directly scalable - These ops do not contain any child ops. They do
have a parameter for directly scaling their throuhgput. Examples of these ops
are linebuffers and space-time type reshapers. 

3. Parent, indirectly scalable - These ops contain child ops which can be
directly or indirectly scaled. These ops do not have a parameter for directly
scaling their throughput. Examples of these ops are composeSeq and composePar.

4. Parent, directly scalable - These ops both do contain child ops and have a
parameter for directly scaling their throughput. Examples of these ops are map
and reduce. There may be restrictions on the types of ops that can be children
of these ops. These restrictions enable the throughput parameter to be modified
while ensuring that the input and output remains the same modulo throughput.

The four groups are have their throughputs increased and decreased using
different approaches:

1. Leaf, indirectly scalable - Since these aren't directly scalable, they are
sped up and slowed down by wrapping them in a parent, directly scalable op such
as map and underutil.

2. Leaf, directly scalable - Since these are directly scalable, they are sped up
and slowed down by trying to adjust their throughput parameters. In some cases,
it may not be possible to adjust the parameter. This will happen if the op with
the new parameter is invalid given the dimensions of the data being operated on.
Speed up and slow down may fail in these cases.

3. Parent, indirectly scalable - Since these aren't directly scalable, they are
sped up and slowed down by trying to scale their children.

4. Parent, directly scalable - Since these are directly scalable, they are sped
up and slowed down by trying to adjust their throughput parameters. If that is
not possible (for the same reasons as case #2), then speed up and slow down try
to scale the throughputs of their child ops.
-}
module Aetherling.Passes.ThroughputModifications (speedUp, slowDown) where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics
import Data.Ratio

-- | Increase the throughput of an Aetherling DAG by increasing area and
-- utilization. speedUp attempts to increase throughput by increasing
-- utilization before using more area.
speedUp requestedMult op | actualMult == requestedMult = spedUpOp
  where (spedUpOp, actualMult) = attemptSpeedUp requestedMult op
speedUp requestedMult op = Failure
  (InvalidThroughputModification requestedMult actualMult) 
  where (spedUpOp, actualMult) = attemptSpeedUp requestedMult op

-- This is the helper function that speeds up an op as much as possible
-- and returns the amount sped up.
-- it is split up into the four types of ops described at the top of the file.

attemptSpeedUp :: Int -> Op -> (Op, Int)
-- LEAF, INDIRECTLY SCALABLE 
-- can't change throughput parameter, can't speed up child ops, so just wrap in
-- a map to parallelize
-- ASSUMPTION: the user has specified a basic unit of data that these types
-- operate over. changing this data type will change the meaning of the program.
-- For example, operating over 3 color channel, 8 bit depth image data should
-- has a base type of T_Array 3 (T_Array 8 T_Bit). Speed up and slow down will
-- not change the type because that would change the semantics of the program.
-- The passes will make the program handle greater of fewer of these RGB pixels
-- per clock.
attemptSpeedUp requestedMult op@(Add) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Sub) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Mul) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Div) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Max) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Min) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Ashr _) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Shl _) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Abs) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Not) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(NotInt) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(And) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(AndInt) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Or ) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(OrInt) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(XOr) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(XOrInt) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Eq = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Neq = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Lt = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Leq = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Gt = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@Geq = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(LUT _) = (MapOp requestedMult op, requestedMult)

-- Must map over these because making a single memory wider is a more
-- expensive operation than banking it (making multiple copies)
-- NOTE: unclear what expensive means here, just my understanding from
-- talking with Spatial team, Ross, and other hardware people. I should
-- figure it out.
-- maping over a MemRead/MemWrite is banking it, and wrapping
-- the type with an array is making a single memory read/write more
-- per clock.
attemptSpeedUp requestedMult op@(MemRead _) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(MemWrite _) = (MapOp requestedMult op, requestedMult)

-- These are leaf, indirectly scalable unlike SequenceArrayRepack because,
-- for these operations, the user has not specified the type separately from the
-- array length. Therefore, speedup may modify the meaning of the program
-- by changing the types of these ops.
attemptSpeedUp requestedMult op@(ArrayReshape _ _) =
  (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(DuplicateOutputs _ _) =
  (MapOp requestedMult op, requestedMult)

attemptSpeedUp requestedMult op@(Constant_Int _) = (MapOp requestedMult op, requestedMult)
attemptSpeedUp requestedMult op@(Constant_Bit _) = (MapOp requestedMult op, requestedMult)

-- LEAF, DIRECTLY SCALABLE
-- If possible, speed up by changing the throughput parameter. Otherwise, just
-- try to return an op with the same or greater throughput than the original.
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
{-
attemptSpeedUp requestedMult (LineBuffer p w img t bc) =
  (LineBuffer (reverse reversedNewP) w img t bc, actualMult)
  where
    (reversedNewP, actualMult) =
      increaseLBPxPerClock (reverse p) (reverse img) requestedMult
-}

-- speeding up SequenceArrayRepack means handling streams with
-- more bits per element of the stream.
-- For example, attemptSpeedUp 2 $ SequenceArrayRepack (4, 1) (2, 2) T_Int ==
-- SequenceArrayRepack (4, 2) (2, 4) T_Int
-- could decrease sLenIn and sLenOut as increase throughput, but not going
-- to do that as don't want to deal with fractional sequence lengths,
-- which could happen if dividing sLenIn or sLenOut
--
-- Akeley: Just passing through cps field for now without modification
-- to make it compile. This should be refined.
attemptSpeedUp requestedMult (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) cps_ t) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn * requestedMult)
    (sLenOut, oldArrLenOut * requestedMult) cps_ t, requestedMult)


-- PARENT, INDIRECTLY SCALABLE
-- Speed up their child ops, no throughput parameter to modify on these, and no
-- point in mapping over these as can just defer that to doing over children.

attemptSpeedUp requestedMult op@(NoOp _) = (MapOp requestedMult op, requestedMult)

attemptSpeedUp requestedMult (ComposePar ops) = 
  let
    spedUpOpsAndMults = map (attemptSpeedUp requestedMult) ops
    spedUpOps = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  in (ComposePar spedUpOps, minimum actualMults)
attemptSpeedUp requestedMult (ComposeSeq ops) = 
  let
    spedUpOpsAndMults = map (attemptSpeedUp requestedMult) ops
    (hdSpedUpOps:tlSpedUpOps) = map fst spedUpOpsAndMults
    actualMults = map snd spedUpOpsAndMults
  -- doing a fold here instead of just making another composeSeq to make sure all
  -- ports still match 
  in (foldl (|>>=|) hdSpedUpOps tlSpedUpOps, minimum actualMults)

-- PARENT, DIRECTLY SCALABLE 
-- speed up the parent by increasing the throughput parameter if possible. If
-- not possible, try to speed up the children.

-- If child op has internal state, can't automatically speed up by
-- changing par and making more copies. Making two independent copies with
-- different state won't behave same as modifying op to update its state to run
-- twice as fast
attemptSpeedUp requestedMult (MapOp par innerOp) | not $ hasInternalState innerOp =
  (MapOp (par*requestedMult) innerOp, requestedMult)
attemptSpeedUp requestedMult (MapOp par innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp requestedMult innerOp
  in (MapOp par spedUpInnerOp, actualMult)

-- speed up reduce based on three cases:
-- 1. child op has internal state: just speed up child op.
-- Due to state, can't make multiple copies of child op for same reason as map
-- 2. child op has no internal state and requestedMult has value so that
-- numTokens >= the amount of parallelism post speed up (newPar):
-- increase parallelism factor to desired amount
-- ASSUMPTION: newPar must cleanly divide into numTokens, or numTokens % newPar == 0
-- 3. child op has no internal state and requestedMult has value so that afterwards
-- numTokens < newPar: make par == numTokens and map over reduce to get rest
-- of parallelism
-- ASSUMPTION: numTokens must cleanly divide into newPar, or
-- newPar % numTokens == 0
-- 4. Child has no internal state, but the other conditions don't hold: speed up
-- the child
attemptSpeedUp requestedMult (ReduceOp numTokens par innerOp) |
  (not $ hasInternalState innerOp) &&
  ((newPar <= numTokens) && ((numTokens `mod` newPar) == 0)) =
  (ReduceOp numTokens newPar innerOp, requestedMult)
  where newPar = par*requestedMult
attemptSpeedUp requestedMult (ReduceOp numTokens par innerOp) |
  (not $ hasInternalState innerOp) &&
  ((newPar > numTokens) && ((newPar `mod` numTokens) == 0)) =
  (MapOp mapPar $ ReduceOp numTokens numTokens innerOp, requestedMult)
  where
    newPar = par*requestedMult
    mapPar = requestedMult `ceilDiv` (numTokens `ceilDiv` par)
attemptSpeedUp requestedMult (ReduceOp numTokens par innerOp) =
  let (spedUpInnerOp, actualMult) = attemptSpeedUp requestedMult innerOp
  in (ReduceOp numTokens par spedUpInnerOp, actualMult)

-- Akeley: Needs to be adapted for fractional underutilization (LogicalUtil).
--
-- -- cases:
-- -- 1. if requestedMult less than or equal to than denom and requestedMult
-- -- divides cleanly into denom, just decrease the underutil denom
-- -- 2. if requestedMult greater than denom and denom divides cleanly into
-- -- requestedMult, remove the underutil and speed up the innerOp using the
-- -- remaining part of requestedMult
-- -- 3. fall back, just speed up the inner op
-- attemptSpeedUp requestedMult (Underutil denom op) |
--   (denom `mod` requestedMult) == 0 =
--   (Underutil (denom `ceilDiv` requestedMult) op, requestedMult)
-- attemptSpeedUp requestedMult (Underutil denom op) |
--   (requestedMult `mod` denom) == 0 =
--   (spedUpOp, denom*innerMult)
--   where
--     remainingMult = requestedMult `ceilDiv` denom
--     (spedUpOp, innerMult) = attemptSpeedUp remainingMult op 
-- attemptSpeedUp requestedMult op@(Underutil denom innerOp) =
--   (Underutil denom innerSpedUpOp, innerMult)
--   where (innerSpedUpOp, innerMult) = attemptSpeedUp requestedMult innerOp

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
slowDown requestedDiv op | actualDiv == requestedDiv = spedUpOp
  where (spedUpOp, actualDiv) = attemptSlowDown requestedDiv op
slowDown requestedDiv op = Failure $ InvalidThroughputModification requestedDiv actualDiv
  where (spedUpOp, actualDiv) = attemptSlowDown requestedDiv op

-- This is the helper function that slows down an op as much as possible
-- and returns the amount slowed down.

-- LEAF, INDIRECTLY SCALABLE 
-- can't change throughput parameter, can't slow child ops, so just wrap in an
-- underutil to slow down. This is the same approach as speed up, but with
-- underutil instead of map, with same assumption regarding not changing the
-- type.
attemptSlowDown :: Int -> Op -> (Op, Int)
attemptSlowDown requestedDiv op@(Add) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Sub) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Mul) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Div) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Max) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Min) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Ashr _) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Shl _) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Abs) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Not) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(NotInt) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(And) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(AndInt) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Or ) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(OrInt) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(XOr) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(XOrInt) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Eq = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Neq = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Lt = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Leq = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Gt = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@Geq = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(LUT _) = (underutil requestedDiv op, requestedDiv)

-- underutil instead of changing type for same reason as speedUp using map,
-- want to do banking instead of making wider memories
attemptSlowDown requestedDiv op@(MemRead _) = (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(MemWrite _) = (underutil requestedDiv op, requestedDiv)

attemptSlowDown requestedDiv op@(ArrayReshape _ _) =
  (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(DuplicateOutputs _ _) =
  (underutil requestedDiv op, requestedDiv)

attemptSlowDown requestedDiv op@(Constant_Int _) =
  (underutil requestedDiv op, requestedDiv)
attemptSlowDown requestedDiv op@(Constant_Bit _) =
  (underutil requestedDiv op, requestedDiv)

-- LEAF, DIRECTLY SCALABLE
-- If possible, slow down by changing the throughput parameter. Otherwise, just
-- try to return an op with the same or slower throughput than the original.
-- Behavior undefined in same case as for attemptSpeedUp.
-- Not worth supporting slowdown using underutil in event of being unable to
-- adjust throughput parameter as can't do symmetric behavior for speed up.
-- While you can wrap underutil around a stateful operator like a linebuffer,
-- you can't wrap a map around it for the reasons discussed in the
-- attemptSpeedUp comments. Thus, to kee

-- Slow down outer most dimension first, then slow down more inner dimensions.
-- see decreaseLBPxPerClock for more information
-- NOTE: decreaseLBPxPerClock expects dimensions from outermost to innermost
-- which matches the order for LineBuffer.
{-
attemptSlowDown requestedDiv (LineBuffer p w img t bc) =
  (LineBuffer newP w img t bc, actualDiv)
  where
    (newP, actualDiv) = decreaseLBPxPerClock p img requestedDiv
-}
-- slowing down SequenceArrayRepack means handling streams with
-- fewer bits per element of the stream.
-- For example, attemptSlowDown 2 $ SequenceArrayRepack (4, 2) (2, 4) T_Int ==
-- SequenceArrayRepack (4, 1) (2, 2) T_Int
-- not going to change SLen in consistency with speed up
-- can only slow down if both array lengths are divisible by requestedDiv
--
-- Akeley: As earlier, pass through cps to make it compile. Probably
-- not correct.
attemptSlowDown requestedDiv (SequenceArrayRepack (sLenIn, oldArrLenIn)
                              (sLenOut, oldArrLenOut) cps_ t) |
  (oldArrLenIn `mod` requestedDiv == 0) && (oldArrLenOut `mod` requestedDiv == 0) =
  (SequenceArrayRepack (sLenIn, oldArrLenIn `ceilDiv` requestedDiv)
    (sLenOut, oldArrLenOut `ceilDiv` requestedDiv) cps_ t, requestedDiv)
attemptSlowDown requestedDiv op@(SequenceArrayRepack _ _ _ _) = (op, 1)

-- PARENT, INDIRECTLY SCALABLE
-- Slow their child ops. No throughput parameter to modify on these and no
-- point in underutilizing these as can just defer that to children.

attemptSlowDown requestedDiv op@(NoOp _) = (MapOp requestedDiv op, requestedDiv)

attemptSlowDown requestedDiv (ComposePar ops) = 
  let
    slowedOpsAndMults = map (attemptSlowDown requestedDiv) ops
    slowedOps = map fst slowedOpsAndMults
    actualDivs = map snd slowedOpsAndMults
  in (ComposePar slowedOps, maximum actualDivs)
attemptSlowDown requestedDiv (ComposeSeq ops) = 
  let
    slowedOpsAndMults = map (attemptSlowDown requestedDiv) ops
    (hdSlowedOps:tlSlowedOps) = map fst slowedOpsAndMults
    actualDivs = map snd slowedOpsAndMults
  -- doing a fold here instead of just making another composeSeq to make sure all
  -- ports still match 
  in (foldl (|>>=|) hdSlowedOps tlSlowedOps, maximum actualDivs)

-- PARENT, DIRECTLY SCALABLE
-- Slow the parent by decreasing the throughput parameter if possible. If not
-- possible, try to slow the children.

-- If child has internal state, can't automatically slow down by changing par.
-- Changing number of child ops is different from running each one at a lower
-- throuhgput when each child op is managing state. This is the same reasoning
-- as speed up.
attemptSlowDown requestedDiv (MapOp par innerOp) | not $ hasInternalState innerOp &&
  (par `mod` requestedDiv == 0) =
  (MapOp (par `ceilDiv` requestedDiv) innerOp, requestedDiv)
attemptSlowDown requestedDiv (MapOp par innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown requestedDiv innerOp
  in (MapOp par slowedInnerOp, actualDiv)

-- slow down reduce based on three cases:
-- 1. child op has internal state: just slow down child op
-- Due to state, can't change number of copies of child op for same reason as map
-- 2. child op has no internal state: decrease parallelism factor to desired
-- amount
-- ASSUMPTION: requestedDiv must cleanly divide into Par, or par % requestedDiv == 0
-- ASSUMPTION: The amount of parallelism post slow down (newPar) must cleanly
-- divide into numTokens, or numTokens % newPar == 0
-- 3. child op has no internal state but other conditions don't hold: slow down
-- the child op
attemptSlowDown requestedDiv (ReduceOp numTokens par innerOp) |
  (not $ hasInternalState innerOp) &&
  (par `mod` requestedDiv == 0) &&
  (numTokens `mod` newPar == 0) =
  (ReduceOp numTokens newPar innerOp, requestedDiv)
  where newPar = par `ceilDiv` requestedDiv
attemptSlowDown requestedDiv (ReduceOp numTokens par innerOp) =
  let (slowedInnerOp, actualDiv) = attemptSlowDown requestedDiv innerOp
  in (ReduceOp numTokens par slowedInnerOp, actualDiv)

attemptSlowDown requestedDiv (LogicalUtil ratio op) =
  let newRatio = numerator ratio % (denominator ratio * requestedDiv)
  in (LogicalUtil newRatio op, requestedDiv)

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
