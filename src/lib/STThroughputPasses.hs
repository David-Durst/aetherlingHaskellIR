module STThroughputPasses where
import STTypes
import STAST
import STMetrics
import STAnalysis

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
speedUpIfPossible op@(LineBuffer p w _ _) throughMult = (op, 1)
  where
    -- from inner most to outer most, speed up and move on to next when saturate whole dimension
    newP = foldr (\(oldPEl, wEl) (newPList, throughMultRemaining) ->
                    speedUpAndRemainder oldPEl wEl throughMultRemaining )
    -- speed up one dimesnion as much as possible
    speedUpOneDim oldPEl wEl throughMultRemaining = throughMultRemaining `ceilDiv` (wEl `ceilDiv` pEl)
    -- get one dim sped up as much as possible and the remainig amount of speedup
    speedUpAndRemainder oldPEl wEl throughMultRemaining =
      (speedUpOneDim oldPEl wEl throughMultRemaining, throughMultRemaining `ceilDiv` speedUpOneDim oldPEl wEl throughMultRemaining)
