module STThroughputPasses where
import STTypes
import STAST
import STAnalysis

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

