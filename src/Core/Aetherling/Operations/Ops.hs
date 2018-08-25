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
import Aetherling.LineBufferManifestoModule
import Aetherling.Analysis.PortsAndThroughput
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
scaleUtil ratio op =
  if denominator (((cps op)%1) / ratio) /= 1 then
    Failure $ UtilFailure
      ("Needed cps of `" ++ show op ++ "`\
       \ divided by utilRatio " ++ show ratio ++ " to be an integer.")
  else
    LogicalUtil ratio op 

-- Function for wrapping an op in a ready-valid interface.
readyValid :: Op -> Op
readyValid op = ReadyValid op
