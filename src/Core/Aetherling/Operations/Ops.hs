{-|
Module: Aetherling.Operations.Ops
Description: Functions for creating ops or simple patterns of ops.

It's expected that these functions will provide a safer (checked) and
more stable interface for creating pipelines than directly
instantiating Op instances.
-}
module Aetherling.Operations.Ops where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Compose
import Aetherling.Operations.ReadyValid
import Aetherling.LineBufferManifestoModule
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Analysis.Latency
import Data.Ratio

-- SIMD arithmetic operators, pass an array type to automatically map the
-- operator to operate on specified array.
addInts :: TokenType -> Op
addInts = mapIntAdapter Add
addI = addInts

subInts :: TokenType -> Op
subInts = mapIntAdapter Sub
subI = subInts

mulInts :: TokenType -> Op
mulInts = mapIntAdapter Mul
mulI = mulInts

divInts :: TokenType -> Op
divInts = mapIntAdapter Div
divI = divInts

maxInts :: TokenType -> Op
maxInts = mapIntAdapter Max
maxI = maxInts

minInts :: TokenType -> Op
minInts = mapIntAdapter Min
minI = minInts

ashr :: Int -> TokenType -> Op
ashr shift = mapIntAdapter (Ashr shift)

shl :: Int -> TokenType -> Op
shl shift = mapIntAdapter (Shl shift)

absInts :: TokenType -> Op
absInts = mapIntAdapter Abs
absI = absInts

notBits :: TokenType -> Op
notBits = mapBitAdapter Not
notB = notBits

notInts :: TokenType -> Op
notInts = mapIntAdapter NotInt
notI = notInts

andBits :: TokenType -> Op
andBits = mapBitAdapter And
andB = andBits

andInts :: TokenType -> Op
andInts = mapIntAdapter AndInt
andI = andInts

orBits :: TokenType -> Op
orBits = mapBitAdapter Or
orB = orBits

orInts :: TokenType -> Op
orInts = mapIntAdapter OrInt
orI = orInts

xorBits :: TokenType -> Op
xorBits = mapBitAdapter XOr
xorB = xorBits

xorInts :: TokenType -> Op
xorInts = mapIntAdapter XOrInt
xorI = xorInts

eq :: TokenType -> Op
eq = mapIntAdapter Eq

neq :: TokenType -> Op
neq = mapIntAdapter Neq

lt :: TokenType -> Op
lt = mapIntAdapter Lt

gt :: TokenType -> Op
gt = mapIntAdapter Gt

leq :: TokenType -> Op
leq = mapIntAdapter Leq

geq :: TokenType -> Op
geq = mapIntAdapter Geq

-- LUT creation function. Pass in the (0-indexed) lookup table.
lut :: [Int] -> Op
lut table = LUT table

mapIntAdapter :: Op -> TokenType -> Op
mapIntAdapter rawOp T_Int = rawOp
mapIntAdapter rawOp (T_Array n t) = MapOp n (mapIntAdapter rawOp t)
mapIntAdapter rawOp t =
  error (show rawOp ++ " does not accept " ++ show t ++ " input.")

mapBitAdapter :: Op -> TokenType -> Op
mapBitAdapter rawOp T_Bit = rawOp
mapBitAdapter rawOp (T_Array n t) = MapOp n (mapIntAdapter rawOp t)
mapBitAdapter rawOp t =
  error (show rawOp ++ " does not accept " ++ show t ++ " input.")


-- Function for making a line buffer (based on The Line Buffer Manifesto).
manifestoLineBuffer :: (Int, Int) -> (Int, Int) -> (Int, Int)
                    -> (Int, Int) -> (Int, Int) -> TokenType
                     -> Op
manifestoLineBuffer pxPerClk window image stride origin token =
  case manifestoCheckAssumptions
       (ManifestoData pxPerClk window image stride origin token) of
    Left message -> error message
    Right lb -> LineBufferManifesto lb


-- | Create a constant generater producing the given token type and
-- value. Values are passed as a 1D list (for bit values, 0 = False, 0
-- /= True). They will be distributed to the actual array entries in
-- lexicographical order.
genConstant :: TokenType -> [Int] -> Op
genConstant t ints =
  let
    extractLenType T_Int = (1, T_Int)
    extractLenType T_Bit = (1, T_Bit)
    extractLenType T_Unit = error "Can't generate T_Unit"
    extractLenType (T_Array n t) =
      let (n',t') = extractLenType t in (n*n', t')
    (lenExpected, elemType) = extractLenType t
  in
    if length ints /= lenExpected then
      error("Needed " ++ (show lenExpected) ++ " values for "
           ++ (show t) ++ " constant.")
    else if elemType == T_Int then
      Constant_Int ints |>>=| ArrayReshape [T_Array lenExpected T_Int] [t]
    else if elemType == T_Bit then
      Constant_Bit (map (/=0) ints) |>>=|
        ArrayReshape [T_Array lenExpected T_Bit] [t]
    else
      error("Aetherling internal error: unexpected elemType " ++ show elemType)


-- | Reshapes an input array sequence through space and time. Buffers
-- inputs (left-to-right) and emits output only when sufficient
-- outputs are ready (but sometimes a bit later than that).
--
-- Args:
--
-- (Int, Int) -> input sequence length and array width
-- (Int, Int) -> output sequence length and array width
-- TokenType  -> Type of array entries
--
-- In words: SequenceArrayRepack (iSeq, iWidth) (oSeq, oWidth) cps t
-- takes in a sequence of iSeq iWidth-arrays of t and emits a
-- sequence of oSeq oWidth-arrays of t.
--
-- The array entries (t) are treated as an atomic type. If t is itself
-- an array type, its elements will never be broken up and emitted
-- on different clock cycles or in separate arrays.
--
-- The cps of this op is the max of iSeq and oSeq. Use scaleUtil to
-- change this.
sequenceArrayRepack :: (Int, Int) -> (Int, Int) -> TokenType -> Op
sequenceArrayRepack (iSeq, iWidth) (oSeq, oWidth) t =
  if iSeq <= 0 || iWidth <= 0 || oSeq <= 0 || oWidth <= 0 then
    error "Need positive parameters for SequenceArrayRepack."
  else if iSeq*iWidth /= oSeq*oWidth then
    error("SequenceArrayRepack input and output token count don't match.\n" ++
         "Gets " ++ (show (iSeq*iWidth)) ++ " tokens per sequence in.\n" ++
         "Gets " ++ (show (oSeq*oWidth)) ++ " tokens per sequence out.\n")
  else
    SequenceArrayRepack (iSeq, iWidth) (oSeq, oWidth) (max iSeq oSeq) t


-- | Wire up the input wires with the output wires.  Match them by
-- "flattening" the input and output array types and gluing the
-- flattened types together left to right. Example that makes more sense:
--
-- tInts [2,2] -> T_Int, tInts [3] gets flattened to
-- ([0][0], [0][1], [1][0], [1][1]) -> (i) ([0], [1], [2])
--
-- so [0][0] goes to output i, [0][1] to [0], [1][0] to [1], [1][1] to [2].
arrayReshape :: [TokenType] -> [TokenType] -> Op
arrayReshape inTokens outTokens =
  if flattenTypes inTokens /= flattenTypes outTokens then
    Failure $ ArrayReshapeTypeMismatch
        (flattenTypes inTokens) (flattenTypes outTokens)
  else
    ArrayReshape inTokens outTokens
  where
    flattenTypes :: [TokenType] -> [TokenType]
    flattenTypes (T_Array n t:ts) =
      flattenTypes (replicate n t) ++ flattenTypes ts
    flattenTypes (t:ts) = t:flattenTypes ts
    flattenTypes [] = []


-- | Duplicate the outputs of an op, n times. Set n = 0 to
-- discard op outputs.
duplicateOutputs :: Int -> Op -> Op
duplicateOutputs n op =
  if n < 0 then
    error "Cannot have negative duplicateOutputs count."
  else
    DuplicateOutputs n op


-- | Lift a (t.. -> u..) op to work on (array t.. -> array u..).
mapOp :: Int -> Op -> Op
mapOp n op =
  if n <= 0 then
    error "Map must have positive parallelism."
  else
    MapOp n op


-- | Reduce using a binary operator (a, b -> c).
-- First int is number of tokens combined by one reduce operation.
-- Second int is the parallelism: the number of tokens processed
-- in one logical cycle*. If parallelism < numTokens, then
-- numTokens/parallelism must be an integer; this is the number
-- of logical cycles it takes to produce one output token.
--
-- *Not sure if still true for non-combinational reduced op.
reduceOp :: Int -> Int -> Op -> Op
reduceOp numTokens parallelism op =
  if numTokens <= 0 || parallelism <= 0 then
    error "Reduce needs positive parameters."
  else if numTokens `mod` parallelism /= 0 then
    error "numTokens/parallelism must be an integer in reduce."
  else if illegalOp op then
    error "Reduced op must be binary operator with 2 identical in ports."
  else
    ReduceOp numTokens parallelism op
  where
    illegalOp op =
      let [a,b] = inPorts op
      in
        length (inPorts op) /= 2 || length (outPorts op) /= 1 ||
          pTType a /= pTType b || pSeqLen a /= pSeqLen b


-- Function for applying LogicalUtil to an op.
-- | Slow down an op by an integer factor.
underutil :: Int -> Op -> Op
underutil denom
  | denom > 0 = scaleUtil (1%denom)
  | otherwise = error "Cannot underutil by non-positive denominator."


-- | Scale the op's logical speed by the given fraction in (0, 1].
scaleUtil :: Ratio Int -> Op -> Op
scaleUtil 1 op = op
scaleUtil ratio _
  | ratio <= 0 || ratio > 1 =
    Failure $ UtilFailure
      "Can only scale utilization by amount in (0, 1]."
scaleUtil ratio (LogicalUtil originalRatio op) =
  scaleUtil (ratio * originalRatio) op
-- XXX We check at the point that the user requests a scaleUtil that
-- we aren't creating an impossible SequenceArrayRepack (non-integer
-- cps). But what if the repack is buried in something else? Then
-- the user will only get an error after distributeUtil is called
-- implicitly which may be quite confusing.
scaleUtil ratio op@(SequenceArrayRepack iTuple oTuple oldCPS t) =
  let
    newCPS = (oldCPS * denominator ratio) % (numerator ratio)
  in
    if denominator newCPS /= 1 then
      Failure $ UtilFailure
      ("Needed cps of `" ++ show op ++ "`\
      \ divided by utilRatio " ++ show ratio ++ " to be an integer.")
    else
      SequenceArrayRepack iTuple oTuple (numerator newCPS) t
-- For register underutil, just modify the util ratio field.
-- In general we can't determine the correct number of delays
-- afterwards, so set to 1 to avoid false sense of security.
scaleUtil ratio op@(Register _ oldUtil t) =
  Register 1 (ratio*oldUtil) t
scaleUtil ratio op =
  LogicalUtil ratio op


-- | Gate the inputs of an op with registers (n of them, back-to-back).
regInputs :: Int -> Op -> Op
regInputs n failure@(Failure _) = failure
regInputs n op =
  let
    cps_ = cps op
    registers = [Register n (pSeqLen port % cps_) (pTType port)
                | port <- inPorts op]
    result =
      case inPortsReadyValid op of
        Just True ->
          readyValid (foldr1 (|&|) registers) |>>=| op
        Just False ->
          foldr1 (|&|) registers |>>=| op
        Nothing ->  -- No inputs to delay.
          op
  in
    case result of
      fail@(Failure _) ->
        error("Aetherling internal error: regInputs " ++ (show fail))
      result' -> result'


-- | Gate the outputs of an op with registers (n of them, back-to-back).
regOutputs :: Int -> Op -> Op
regOutputs n failure@(Failure _) = failure
regOutputs n op =
  let
    cps_ = cps op
    registers = [Register n (pSeqLen port % cps_) (pTType port)
                | port <- outPorts op]
    result =
      case outPortsReadyValid op of
        Just True ->
          op |>>=| readyValid (foldr1 (|&|) registers)
        Just False ->
          op |>>=| foldr1 (|&|) registers
        Nothing ->  -- No outputs to delay.
          op
  in
    case result of
      fail@(Failure _) ->
        error("Aetherling internal error: regOutputs " ++ (show fail))
      result' -> result'


-- Function for wrapping an op in a ready-valid interface.
readyValid :: Op -> Op
readyValid op = ReadyValid op
