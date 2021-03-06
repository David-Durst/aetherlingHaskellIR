-- Module describing my (Akeley's) proposed solution to phase
-- (repeating pattern of real and garbage outputs) with fractional
-- underutil.
--
-- So at the moment there's 4 sources of underutilization.
-- 1. ReduceOp (par < numTokens)
-- 2. line buffers (stride /= (1, 1))
-- 3. LogicalUtil
-- 4. SequenceArrayRepack
--
-- Let's think about the performance considerations raised by each.
--
-- ReduceOp and line buffer only cause integer underutil, which
-- naturally creates a pattern of 1 real output followed by N-1
-- garbage cycles (1/N utilization).
--
-- LogicalUtil (underutilization op) is pure waste, so it doesn't
-- really matter what pattern we adopt for it.
--
-- This leaves SequenceArrayRepack as the only meaningful source of
-- fractional underutilization; LogicalUtil can just adopt whatever
-- pattern SequenceArrayRepack uses.
--
-- I adopt the convention that the phase pattern for an x%y [y>x]
-- utilization ratio stream should always be the input pattern
-- expected by a SequenceArrayRepack that converts an x-sequence of
-- y-arrays to a y-sequence of x-arrays ("narrowing" repack) over y
-- clock cycles. This pattern is itself defined with the rule "every
-- input array should come as soon as it is needed, but no sooner".
--
-- Example: 3%5 pattern
-- We construct a (3, 5) (5, 3) SequenceArrayRepack and
-- match up input to outputs.
--
-- ‖ clk 0 ‖ clk 1 ‖ clk 2 ‖ clk 3 ‖ clk 4 ‖ Clock cycles (wide colmuns)
-- ‖ ___   ‖ _     ‖       ‖ __    ‖       ‖
-- ‖ 01234 ‖ 56789 ‖ xxxxx ‖ ABCDE ‖ xxxxx ‖ Tokens (in)
-- ‖ ||||| ‖ ||||└-------┐ ‖ ||||| ‖       ‖
-- ‖ ||||| ‖ |||└------┐ | ‖ ||||└-----┐   ‖
-- ‖ ||||| ‖ ||└--┐‖   | | ‖ |||└-----┐|   ‖
-- ‖ ||||| ‖ |└--┐|‖   | | ‖ ||└-----┐||   ‖
-- ‖ ||||| ‖ └-┐ ||‖   | | ‖ |└┐   ‖ |||   ‖
-- ‖ ||||└----┐| |└---┐| | ‖ └┐|   ‖ |||   ‖
-- ‖ |||└----┐|| └---┐|| └---┐||   ‖ |||   ‖
-- ‖ |||   ‖ |||   ‖ |||   ‖ |||   ‖ |||   ‖
-- ‖ 012   ‖ 345   ‖ 678   ‖ 9AB   ‖ CDE   ‖ Tokens (out)
-- ‖ 000   ‖ 110   ‖ 111   ‖ 200   ‖ 111   ‖ Token latency
--
-- Notice how none of the outputs (numbered 0-E) had to go backwards
-- in time (so each input 5-array came as soon as it was needed) but
-- each of the 3 input 5-arrays had at least one element emitted on
-- the same clk cycle (but no sooner) – these elements have an
-- overbar over them. Since the 3 arrays came in at cycles 0, 1, and
-- 3, the phase pattern is 0, 1, 3.
--
-- Note that for 1%N ratios (integer underutil), this rule creates
-- the same 1 token / N-1 garbage pattern as earlier.
--
-- Since SequenceArrayRepack is the only relevant op for determining
-- phase patterns, it makes sense to adopt a standardized phase
-- pattern for fractional ratios. This contains the complexity of
-- phase matching to SequenceArrayRepack itself annd allows the rest
-- of the system to just reason with fractional throughputs without
-- worrying about phase.
--
-- This phase pattern may appear to be biased against the opposite
-- SequenceArrayRepack ("widening" repack, e.g. (5, 3) -> (3, 5)).
-- I'm convinced that forcing widening repacks to base its output
-- phase pattern on what's most convenient for a narrowing repack will
-- cause the average latency of each token to go up by no more than 1,
-- and the worst case latency not to increase at all.  (Compared to
-- the "natural" phase pattern "emit an output array as soon as you
-- are able to"). I don't have a proof yet though.
--
-- In any event, forcing this consistent pattern prevents us from
-- having to place complex buffers elsewhere in the circuit
-- to match up different phase patterns. This should save us both
-- design time and chip area.
--
-- An unfortunate fact is that SequenceArrayRepack needs to have a
-- tight relationship with underutilization. This is why I've decided
-- to include a cps field within the SequenceArrayRepack op itself.
-- The reason for this is that underutilizing a SequenceArrayRepack
-- by "skipping" unused clock cycles does not produce the behavior
-- required by the phase patterns specified above.
--
-- (Complicated example incoming).
--
-- Example: Suppose we convert 4-sequence of 3-arrays to 3 4-arrays.
--
-- ‖ clk 0'‖ clk 1'‖ clk 2'‖ clk 3'‖ Clock cycles (wide columns)
-- ‖       ‖       ‖       ‖       ‖
-- ‖ 012   ‖ 345   ‖ 678   ‖ 9AB   ‖ Tokens (in)
-- ‖ |||   ‖ ||└--┐‖ |||   ‖ |||   ‖
-- ‖ |||   ‖ |└---┐‖ ||└-┐ ‖ ||└┐  ‖
-- ‖ |||   ‖ └--┐||‖ |└-┐| ‖ |└┐|  ‖
-- ‖ ||└-------┐|||‖ └-┐|| ‖ └┐||  ‖
-- ‖ |└-------┐|||└---┐||| ‖  |||  ‖
-- ‖ └-------┐|||└---┐|||└---┐|||  ‖
-- ‖       ‖ ||||  ‖ ||||  ‖ ||||  ‖
-- ‖ xxxx  ‖ 0123  ‖ 4567  ‖ 89AB  ‖ Tokens (out)
-- ‖       ‖ 1110  ‖ 1100  ‖ 1000  ‖ Token Latency
--
-- Now do the same thing, but spread out over 6 clocks. The input is
-- 4-sequence over 6 clocks. By the rules above they should come in on
-- cycles 0, 1, 3, and 4. The 3-sequence output should come cycles 1,
-- 3, and 5. (Phase is 0, 2, 4; +1 latency).
--
-- ‖ clk 0 ‖ clk 1 ‖ clk 2 ‖ clk 3 ‖ clk 4 ‖ clk 5 ‖ Clock cycles
-- ‖       ‖       ‖       ‖       ‖       ‖       ‖
-- ‖ 012   ‖ 345   ‖ xxx   ‖ 678   ‖ 9AB   ‖ xxx   ‖ Tokens (in)
-- ‖       ‖       ‖       ‖       ‖       ‖       ‖
-- ‖ xxxx  ‖ 0123  ‖ xxxx  ‖ 4567  ‖ xxxx  ‖ 89AB  ‖ Tokens (out)
-- ‖       ‖ 1110  ‖       ‖ 2200  ‖       ‖ 2111  ‖ Token Latency (real)
-- ‖       ‖       ‖       ‖       ‖       ‖       ‖
-- ‖ clk 0'‖ clk 1'‖       ‖ clk 2'‖    clk 3'?    ‖
--
-- There's actually no way to match up the 4-clocks-per-sequence
-- example with the 6-cps example by idling for 2 cycles, as underutil
-- conceptually does. Notice how input 9AB and output 89AB are on the
-- same cycle (3') in the original but on different cycles (4, 5) in
-- the underutilized version, and latency can not be reduced any
-- further to fix this.
--
-- So, SequenceArrayRepack has to have knowledge of its actual speed
-- in a circuit. Once again I think this complexity is worth it to
-- contain the far greater complexity of phase matching to the
-- SequenceArrayRepack operator.

module Aetherling.Analysis.Phase (
  boolPhase,
  fillPhase,
  fillPhaseInts,
  phaseWhichCycle,
  repackLatency
) where
import Data.List
import Data.Ratio
import Aetherling.Analysis.Metrics


-- All of these functions are incredibly inefficient. It would be nice
-- to try caching or a closed-form solution, but for now it is what it is.

-- | Given a utilization ratio, determine the phase pattern as a list of
-- bools (length = denominator). True indicates a valid input/output is
-- expected on that cycle, False for garbage.
boolPhase :: Ratio Int -> [Bool]
boolPhase ratio
  | ratio > 1 || ratio <= 0 = error "Util ratio needs to be in (0, 1]."
  | otherwise = boolPhaseImpl ratio 0 0

boolPhaseImpl :: Ratio Int -> Int -> Int -> [Bool]
boolPhaseImpl ratio inCount outCount
  -- We define the phase pattern as the input pattern expected by a
  -- (num, denom) to (denom, num) SequenceArrayRepack (cps = denom).
  -- inCount is the number of input arrays so far.
  -- outCount is the number of output arrays emitted so far.
  -- (both not counting this cycle).
  | outCount > denominator ratio =
    error "Aetherling internal error: phase overflowed."
  | outCount == denominator ratio = [] -- cps is denominator.
  | otherwise =
    let
      inWidth = denominator ratio
      outWidth = numerator ratio
      tokensAhead = inCount*inWidth - outCount*outWidth
    in
      if tokensAhead < outWidth then
        -- Would run out if we emitted an outWidth-array this cycle
        -- without reading a new one in.
        True:boolPhaseImpl ratio (inCount+1) (outCount+1)
      else
        -- We always emit an output each cycle. seqLen = cps.
        False:boolPhaseImpl ratio inCount (outCount+1)


-- | Given a utilization ratio, determine the cumulative number of
-- inputs seen on each clock cycle. Return as Int list mapping clk
-- cycle number to input count.
--
-- Example: fillPhase (2%3) is [1, 2, 2], because on cycle 0, 1 real
-- input has been seen so far, and on cycles 1 and 2, 2 inputs will
-- have been seen (since the last cycle's input is garbage).
fillPhase :: Ratio Int -> [Int]
fillPhase ratio
  | ratio > 1 || ratio <= 0 = error "Util ratio needs to be in (0, 1]."
  | otherwise = fillPhaseImpl (numerator ratio) (denominator ratio) 0 0

-- | Like fillPhase, but we explicitly specify the sequence length (in
-- valid token count) and the clocks-per-sequence. This may be useful
-- when you don't want ratio's fraction simplification behavior.
-- (e.g. fillPhase 4 6 is [1,2,2,3,4,4], not [1,2,2]).
fillPhaseInts :: Int -> Int -> [Int]
fillPhaseInts tokenCount cps
  | tokenCount > cps || tokenCount <= 0 =
    error "tokenCount needs to be in (0, cps]."
  | otherwise = fillPhaseImpl tokenCount cps 0 0

fillPhaseImpl :: Int -> Int -> Int -> Int -> [Int]
fillPhaseImpl tokenCount cps inCount outCount
  | outCount > cps =
    error "Aetherling internal error: phase overflowed."
  | outCount == cps = []
  | otherwise =
    let
      inWidth = cps
      outWidth = tokenCount
      tokensAhead = inCount*inWidth - outCount*outWidth
    in
      if tokensAhead < outWidth then
        -- Would run out if we emitted an outWidth-array this cycle
        -- without reading a new one in.
        inCount + 1:fillPhaseImpl tokenCount cps (inCount+1) (outCount+1)
      else
        -- We always emit an output each cycle. seqLen = cps.
        inCount:fillPhaseImpl tokenCount cps inCount (outCount+1)


-- | Given a utilization ratio and the index of an output, find out on
-- which cycle the output will actually arrive (assuming 0 latency).
-- e.g. for 2%3 utilization, outputs 0, 1, 2, 3, 4 will come on cycles
-- 0, 1, 3, 4, 6.
phaseWhichCycle :: Ratio Int -> Int -> Int
phaseWhichCycle ratio n =
  let
    cps = denominator ratio
    seqLen = numerator ratio
    seqCount = n `div` seqLen
    seqIndex = n `mod` seqLen
  in
    -- Wow this must be slow.
    case elemIndex (1+seqIndex) (fillPhase ratio) of
      Nothing ->
        error "Aetherling internal error: Phase pattern lookup failure."
      Just phaseIndex ->
        phaseIndex + cps*seqCount


-- | Determine the latency of a SequenceArrayRepack with given parameters.
-- We define latency as the difference in clock cycles between when the
-- first input comes in and the first output goes out.
--
-- Parameters: input sequence length, output sequence length, clocks per sequence.
--
-- Again this is an absurdly inefficient function, suitable for a
-- prototype only.
repackLatency :: Int -> Int -> Int -> Int
repackLatency iSeqLen oSeqLen cps =
  -- Assume that the SequenceArrayRepack has 0 latency. For each
  -- token that passes through the repack (number them 0, 1..),
  -- determine which clock cycle the token was input to or
  -- emitted by the repack. For each token, determine by how many
  -- clock cycles it violated causality. The worst violation is
  -- the latency of the actual repack op.
  let
    -- We already know our exact input/output phase patterns per the
    -- header comment.
    iFillPhase = fillPhaseInts iSeqLen cps
    oFillPhase = fillPhaseInts oSeqLen cps

    -- Can just assume these complimentary array widths.
    gcd' = gcd iSeqLen oSeqLen
    iWidth = oSeqLen `div` gcd'
    oWidth = iSeqLen `div` gcd'
    totalTokens = oWidth * oSeqLen

    -- Make lists associating token number with its input/output
    -- cycle. e.g. iSchedule !! 5 = 3 means that the 5th token
    -- came in on the 3rd cycle (0-indexed in both cases).
    makeSchedule :: Int -> [Int] -> Int -> Int -> [Int]
    makeSchedule arrayWidth fillPhaseLeft clkIndex tokenIndex
      | tokenIndex == totalTokens = []
      | otherwise =
        if arrayCount*arrayWidth > tokenIndex then
          -- We're creating the the `tokenIndex`th entry of the final
          -- list here. Since according to the fill phase, there's
          -- been enough input/output at the `clkIndex`th clock to
          -- have seen the `tokenIndex`th token, set
          -- schedule!tokenIndex to clkIndex.
          clkIndex:
            (makeSchedule arrayWidth fillPhaseLeft clkIndex (tokenIndex+1))
        else
          -- Not enough input, maybe there's enough on the next cycle?
          makeSchedule arrayWidth (tail fillPhaseLeft) (clkIndex+1) tokenIndex
        where
          arrayCount = head fillPhaseLeft

    iSchedule = makeSchedule iWidth iFillPhase 0 0
    oSchedule = makeSchedule oWidth oFillPhase 0 0
  in
    -- Figure out N = worst causality violation. That N is the amount
    -- of latency in our actual SequenceArrayRepack needed to avoid
    -- said causality violation.
    maximum [iTokenClk - oTokenClk
            | (iTokenClk, oTokenClk) <- zip iSchedule oSchedule]
