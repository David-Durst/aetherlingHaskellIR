module Time where
import SpaceTime.STTypes
import STMetrics
import STAST

cps op = clocksPerSequence op
clocksPerSequence :: Op -> Int
registerCPS = 1 
combinationalCPS = 1

clocksPerSequence (Add t) = combinationalCPS
clocksPerSequence (Sub t) = combinationalCPS
clocksPerSequence (Mul t) = combinationalCPS
clocksPerSequence (Div t) = combinationalCPS
clocksPerSequence (Max t) = combinationalCPS
clocksPerSequence (Min t) = combinationalCPS
clocksPerSequence (Ashr _ t) = combinationalCPS
clocksPerSequence (Shl _ t) = combinationalCPS
clocksPerSequence (Abs t) = combinationalCPS
clocksPerSequence (Not t) = combinationalCPS
clocksPerSequence (And t) = combinationalCPS
clocksPerSequence (Or t) = combinationalCPS
clocksPerSequence (XOr t) = combinationalCPS
clocksPerSequence Eq = combinationalCPS
clocksPerSequence Neq = combinationalCPS
clocksPerSequence Lt = combinationalCPS
clocksPerSequence Leq = combinationalCPS
clocksPerSequence Gt = combinationalCPS
clocksPerSequence Geq = combinationalCPS
clocksPerSequence (LUT _) = combinationalCPS

-- to what degree can we pipeline MemRead and MemWrite
clocksPerSequence (MemRead _) = combinationalCPS 
clocksPerSequence (MemWrite _) = combinationalCPS 

clocksPerSequence (LineBuffer (pHd:[]) _ (imgHd:[]) t _) = imgHd `ceilDiv` pHd
clocksPerSequence (LineBuffer (pHd:pTl) (_:wTl) (imgHd:imgTl) t bc) =
  (imgHd `ceilDiv` pHd) * (cps $ LineBuffer pTl wTl imgTl t bc)
clocksPerSequence (LineBuffer _ _ _ _ _) = -1
clocksPerSequence (Constant_Int _) = combinationalCPS
clocksPerSequence (Constant_Bit _) = combinationalCPS

-- Assuming either the input or output is fully utilized (dense), the
-- clocks taken per sequence is just the longer sequence of the two.
clocksPerSequence (SequenceArrayRepack (inSeq, _) (outSeq, _) _) =
  max inSeq outSeq
clocksPerSequence (ArrayReshape _ _) = combinationalCPS
clocksPerSequence (DuplicateOutputs _ _) = combinationalCPS

clocksPerSequence (MapOp _ op) = cps op
-- if reducing combinational operator, clocks is number of iterations
-- reduce needs to get a complete sequence. If less than parallel,
-- need to write to register all but last, if fully parallel or more,
-- reduce is combinational
clocksPerSequence (ReduceOp par numComb op) |
  isComb op = combinationalCPS * (numComb `ceilDiv` par)
-- Why not including tree height? Because can always can pipeline.
-- Putting inputs in every clock where can accept inputs.
-- Just reset register every numComb/par if not fully parallel.
-- What does it mean to reduce a linebuffer?
-- can't. Can't reduce anything with a warmup as this will create
-- an asymmetry between inputs and outputs leading to horrific tree
-- structure
clocksPerSequence (ReduceOp par numComb op) = cps op * (numComb `ceilDiv` par)

clocksPerSequence (NoOp _) = combinationalCPS
clocksPerSequence (Underutil denom op) = denom * cps op
-- since pipelined, this doesn't affect clocks per stream
clocksPerSequence (Delay _ op) = cps op

-- will handle fact of doing max warmup in port sequence lengths, not here.
-- here we just make all the times match up, worry about what to do during
-- those times in port seq len
clocksPerSequence (ComposePar ops) = foldl lcm 1 $ map cps ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = foldl lcm 1 $ map cps ops
clocksPerSequence (Failure _) = -1


registerInitialLatency = 1
initialLatency :: Op -> Int
initialLatency (Add t) = 1
initialLatency (Sub t) = 1
initialLatency (Mul t) = 1
initialLatency (Div t) = 1
initialLatency (Max t) = 1
initialLatency (Min t) = 1
initialLatency (Ashr _ t) = 1
initialLatency (Shl _ t) = 1
initialLatency (Abs t) = 1
initialLatency (Not t) = 1
initialLatency (And t) = 1
initialLatency (Or t) = 1
initialLatency (XOr t) = 1
initialLatency Eq = 1
initialLatency Neq = 1
initialLatency Lt = 1
initialLatency Leq = 1
initialLatency Gt = 1
initialLatency Geq = 1
initialLatency (LUT _) = 1

initialLatency (MemRead _) = 1
initialLatency (MemWrite _) = 1
-- intiial latency is just number of warmup clocks
initialLatency lb@(LineBuffer p w _ _ _) = 1 
initialLatency (Constant_Int _) = 1
initialLatency (Constant_Bit _) = 1

initialLatency (SequenceArrayRepack (inSeq, _) (outSeq, _) _) =
  outSeq `ceilDiv` inSeq
initialLatency (ArrayReshape _ _) = 1
initialLatency (DuplicateOutputs _ _) = 1

initialLatency (MapOp _ op) = initialLatency op
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 && isComb op = 1
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 = initialLatency op * (ceilLog par)
initialLatency (ReduceOp par numComb op) =
  -- pipelinng means only need to wait on latency of tree first time
  reduceTreeInitialLatency + (numComb `ceilDiv` par) * (initialLatency op + registerInitialLatency)
  where 
    reduceTreeInitialLatency = initialLatency (ReduceOp par par op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (initialLatency op) (isComb op)


initialLatency (NoOp _) = 0
initialLatency (Underutil denom op) = initialLatency op
-- since pipelined, this doesn't affect clocks per stream
initialLatency (Delay dc op) = initialLatency op + dc

initialLatency (ComposePar ops) = maximum $ map initialLatency ops
-- initialLatency is 1 if all elemetns are combintional, sum of latencies of sequential
-- elements otherwise
initialLatency (ComposeSeq ops) = bool combinationalInitialLatency sequentialInitialLatency
  (sequentialInitialLatency > 0)
  where 
    combinationalInitialLatency = 1
    sequentialInitialLatency = foldl (+) 0 $ map initialLatency $ filter (not . isComb) ops
initialLatency (Failure _) = 0
