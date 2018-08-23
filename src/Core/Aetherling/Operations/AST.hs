{-|
Module: Aetherling.Operations.AST
Description: Aetherling's AST

Provides the Aetherling Abstract Syntax Tree (AST) and functions
for identifying errors in that tree.
-}
module Aetherling.Operations.AST where
import Aetherling.Operations.Types
import Aetherling.LineBufferManifestoModule
import Data.Ratio

-- | The operations that can be used to create dataflow DAGs in Aetherling
data Op =
  -- LEAF OPS
  Add
  | Sub
  | Mul
  | Div
  | Max
  | Min
  | Ashr Int
  | Shl Int
  | Abs
  | Not
  | NotInt
  | And
  | AndInt
  | Or
  | OrInt
  | XOr
  | XOrInt
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  -- | Ints are the entries in the table. First entry is index 0
  | LUT [Int]
  | MemRead TokenType
  | MemWrite TokenType
  -- | first arg is pixels per clock in each dimension. First value in list is
  -- outer most dimension that iterating over (rows first, columns second in 2d
  -- case). second arg is window width in each dimension. Same indexing order.
  -- third arg is the size of the image. Saem indexing order. This is necessary
  -- for internal buffer sizing
  -- Last is the type of the pixel element
  | LineBuffer {pxPerClock :: [Int], windowWidth :: [Int], image :: [Int],
                lbInT :: TokenType, boundaryCondition :: BoundaryConditions}

  -- Temporary line buffer op based on semantics in "The Line Buffer
  -- Manifesto".  I need a working line buffer to make progress on my
  -- work. Later when we refine our main line buffer to work I can go back
  -- and port uses of LineBufferManifesto to the main line buffer.
  | LineBufferManifesto ManifestoData

  -- | Array is constant produced, int is sequence length
  | Constant_Int {intConstProduced :: [Int]}
  -- | Array is constant produced
  | Constant_Bit {bitConstProduced :: [Bool]}

  -- TYPE MANIPULATORS
  --
  -- | Reshapes an input array sequence through space and time.  Buffers
  -- inputs (left-to-right) and emits output only when sufficient
  -- outputs are ready.  Args: input tuple, output tuple, array entry
  -- type. Tuple consists of (sequence length, array length) for the
  -- one input or one output port. Array type may be another array; if
  -- so, it's treated as atomic and not split between two output
  -- cycles.
  | SequenceArrayRepack (Int, Int) (Int, Int) TokenType
  -- | First is list of input port types, second is output.
  -- Pure combinational device: decomposes the input and output arrays to
  -- a sequence of wires, and wires up inputs to outputs in order.
  | ArrayReshape [TokenType] [TokenType]
  | DuplicateOutputs Int Op

  -- HIGHER ORDER OPS
  | MapOp {mapParallelism :: Int, mappedOp :: Op}
  | ReduceOp {reduceNumTokens :: Int, reduceParallelism :: Int, reducedOp :: Op}

  -- TIMING HELPERS
  | NoOp [TokenType]
  -- | Logically change the (time) utilization of the utilOp.  If
  -- utilRatio is A/B, then utilOp only has "meaningful" inputs and
  -- outputs on A out of B cycles. A/B must be in (0,1], and A/B *
  -- clocks-per-sequence of utilOp must still be an integer.  I
  -- recommend against constructing this op manually; use the helper
  -- functions. "Logical" is meant to emphasize our viewpoint that
  -- this changes our interpretation of an op's outputs, and
  -- LogicalUtil may not have any physical effect on the actual
  -- hardware.
  | LogicalUtil {utilRatio :: Ratio Int, utilOp :: Op}
  | Delay {delayClocks :: Int, delayedOp :: Op}

  -- COMPOSE OPS
  | ComposePar [Op]
  | ComposeSeq [Op]
  | Failure FailureType

  -- Ready-valid meta-op.
  -- Wraps the whole op with a ready-valid interface. Connections
  -- between child ops within the wrapped op are not affected.
  | ReadyValid Op
  deriving (Eq, Show)

-- | The how to handle boundaries where LineBuffer emits invalid data.
-- this is here so that LineBuffer signature doesn't have warmup.
-- The warmup makes synchronously timing the circuit very difficult
-- as need downstream ops to have weird underutilization patterns
-- that are hard to automatically change in a speed up or slow down.
data BoundaryConditions =
  -- | Crop means to have the system automatically remove these values
  -- from the output using an op at the end of the DAG
  Crop
  -- | KeepGarbage means to leave in the outputs during invalid clocks.
  -- The user will handle them.
  | KeepGarbage
  deriving (Eq, Show)

data FailureType =
  ComposeFailure ComposeResult (Op, Op)
  | InvalidThroughputModification {attemptedMult :: Int, actualMult :: Int}
  deriving (Eq, Show)

data ComposeResult = 
  PriorFailure 
  -- | ReadyValidMismatch indicates that we tried to compose a synchronous op
  -- with an op that has a ready-valid interface.
  | ReadyValidMismatch
  | PortCountMismatch
  -- | TokenTypeMismatch indicates that we tried to glue two ports of
  -- different type together.
  | TokenTypeMismatch PortType PortType
  -- | SeqPortMismatch indicates couldn't do comopse as composeSeq
  -- requires all port types and latencies (for reasons besides 2
  -- above -- I think just throughput mismatches?)
  | SeqPortMismatch {outPortsThroughput :: [PortThroughput],
                     inPortsThroughput :: [PortThroughput]}
  | ComposeSuccess
  deriving (Eq, Show)

-- | debugging helper methods for parsing syntax tree
-- get the ops contained inside other ops, for going down ComposeFailure trees
getChildOp n op = getChildOps op !! n
getChildOps :: Op -> [Op]
getChildOps (Add) = []
getChildOps (Sub) = []
getChildOps (Mul) = []
getChildOps (Div) = []
getChildOps (Max) = []
getChildOps (Min) = []
getChildOps (Ashr _) = []
getChildOps (Shl _) = []
getChildOps (Abs) = []
getChildOps (Not) = []
getChildOps (NotInt) = []
getChildOps (And) = []
getChildOps (AndInt) = []
getChildOps (Or) = []
getChildOps (OrInt) = []
getChildOps (XOr) = []
getChildOps (XOrInt) = []
getChildOps (Eq) = []
getChildOps (Neq) = []
getChildOps (Lt) = []
getChildOps (Leq) = []
getChildOps (Gt) = []
getChildOps (Geq) = []
getChildOps (LUT _) = []
getChildOps (MemRead _) = []
getChildOps (MemWrite _) = []
getChildOps (LineBuffer _ _ _ _ _) = []
getChildOps (LineBufferManifesto _) = []
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayRepack _ _ _) = []
getChildOps (ArrayReshape _ _) = []
getChildOps (DuplicateOutputs _ op) = [op]
getChildOps (MapOp _ op) = [op]
getChildOps (ReduceOp _ _ op) = [op]
getChildOps (NoOp _) = []
getChildOps (LogicalUtil _ op) = [op]
getChildOps (Delay _ op) = [op]
getChildOps (ComposePar ops) = ops
getChildOps (ComposeSeq ops) = ops
getChildOps (ReadyValid op) = [op]
getChildOps (Failure (ComposeFailure _ (op0, op1))) = [op0, op1]
getChildOps (Failure _) = []

-- | Walk the failure AST tree and find the deepest cause for the failure,
-- preferring failures on the left over the right
-- Will return the parent node if not failures
isFailure (Failure _) = True
isFailure _ = False
hasChildWithError op = (<) 0 $ length $
  filter (\i -> isFailure i || hasChildWithError i) $ getChildOps op
getFirstError op | hasChildWithError op = head $ map getFirstError $ getChildOps op
getFirstError op | isFailure op = op
getFirstError op = op

-- Convenience functions for creating Ops or simple patterns of Ops.
-- Later, split up into seperate files.

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

-- Function for applying LogicalUtil to an op.
-- | Slow down an op by an integer factor.
underutil :: Int -> Op -> Op
underutil denom
  | denom > 0 = LogicalUtil (1%denom)
  | otherwise = error "Cannot underutil by non-positive denominator."

-- | Scale the op's logical speed by the given fraction in (0, 1].
scaleUtil :: Ratio Int -> Op -> Op
scaleUtil amount
  | amount <= 0 || amount > 1 =
    error "Can only scale utilization by amount in (0, 1]."
  | otherwise =
    LogicalUtil amount

-- Function for wrapping an op in a ready-valid interface.
readyValid :: Op -> Op
readyValid op = ReadyValid op
