{-|
Module: Aetherling.Operations.AST
Description: Aetherling's AST

Provides the Aetherling Abstract Syntax Tree (AST) and functions
for identifying errors in that tree.
-}
module Aetherling.Operations.AST where
import Aetherling.Operations.Types
import Data.Ratio
import GHC.Generics

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
  | LineBuffer {lbData :: LineBufferData}

  -- | Array is constant produced, int is sequence length
  | Constant_Int {intConstProduced :: [Int]}
  -- | Array is constant produced
  | Constant_Bit {bitConstProduced :: [Bool]}

  -- TYPE MANIPULATORS
  --
  -- | Reshapes an input array sequence through space and time. Buffers
  -- inputs (left-to-right) and emits output only when sufficient
  -- outputs are ready (but sometimes a bit later than that).
  --
  -- Args:
  --
  -- (Int, Int) -> input sequence length and array width
  -- (Int, Int) -> output sequence length and array width
  -- Int        -> clocks per sequence
  -- TokenType  -> Type of array entries
  --
  -- In words: SequenceArrayRepack (iSeq, iWidth) (oSeq, oWidth) cps t
  -- takes in a sequence of iSeq iWidth-arrays of t and emits a
  -- sequence of oSeq oWidth-arrays of t, over cps clock cycles.
  --
  -- The array entries (t) are treated as an atomic type. If t is itself
  -- an array type, its elements will never be broken up and emitted
  -- on different clock cycles or in separate arrays.
  | SequenceArrayRepack (Int, Int) (Int, Int) Int TokenType

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
  -- functions. "Logical" is meant to emphasize my viewpoint that
  -- this changes our interpretation of an op's outputs, and
  -- LogicalUtil may not have any physical effect on the actual
  -- hardware (space function does not reflect this view).
  | LogicalUtil {utilRatio :: Ratio Int, utilOp :: Op}

  -- Represents back-to-back registers (regClocks of them) holding
  -- data of regToken type. The regUtil field is just there to satisfy
  -- type-checking (throughput matching). Like SequenceArrayRepack,
  -- Register needs to know its true speed, otherwise the regClocks
  -- meaning is ambiguous: there is a choice between rounding up and
  -- rounding down. (e.g., consider a 3-delay register, utilized by
  -- 2%3. 3 / (2%3) = 4.5; should this be rounded down to a 4-delay
  -- or up to a 5-delay? The correct answer depends on what's going on
  -- in the paths of the circuit parallel to this register,
  -- information we don't have here).
  --
  -- Use functions regInputs, regOutputs instead of using Register directly.
  | Register {regClocks :: Int, regUtil :: Ratio Int, regToken :: TokenType}

  -- COMPOSE OPS
  | ComposePar [Op]
  | ComposeSeq [Op]
  | Failure FailureType

  -- Ready-valid meta-op.
  -- Wraps the whole op with a ready-valid interface. Connections
  -- between child ops within the wrapped op are not affected.
  | ReadyValid Op
  deriving (Eq, Show, Generic)

-- | first arg is pixels per clock in each dimension. First value in list is
-- outer most dimension that iterating over (rows first, columns second in 2d
-- case). second arg is window width in each dimension. Same indexing order.
-- third arg is the size of the image. Same indexing order. This is necessary
-- for internal buffer sizing. Fourth is how far apart each window is from
-- the last one. Origin is the location of the window relative to the pixel
-- that's being moved along the image. Last is the type of the pixel element
data LineBufferData = LineBufferData {
  lbPxPerClk :: (Int, Int),
  lbWindow :: (Int, Int),
  lbImage :: (Int, Int),
  lbStride :: (Int, Int),
  lbOrigin :: (Int, Int),
  lbToken :: TokenType
} deriving (Eq, Show, Generic)

-- The number of parallel window outputs needed.
getLineBufferParallelism :: LineBufferData -> Int
getLineBufferParallelism lb =
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    (strideY, strideX) = lbStride lb
    strideArea = strideY * strideX
  in
    max 1 (div (xPerClk * yPerClk) strideArea)
    
data FailureType =
  ComposeFailure ComposeResult (Op, Op)
  | InvalidThroughputModification {attemptedMult :: Int, actualMult :: Int}
  -- | UtilFailure indicates that the util ratio wasn't appropriate for
  -- the op being underutilized.
  | UtilFailure String
  -- | ArrayReshapeTypeMismatch indicates that it's not possible to wire
  -- up the inputs to the outputs correctly. The fields indicate the
  -- "flattened" types of the input and output token list.
  -- e.g. -- [tInts [3], T_Bit] becomes [T_Int, T_Int, T_Int, T_Bit]
  | ArrayReshapeTypeMismatch
      {flattenedInTokens :: [TokenType], flattenedOutTokens :: [TokenType]}
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
getChildOps (LineBuffer _) = []
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayRepack _ _ _ _) = []
getChildOps (ArrayReshape _ _) = []
getChildOps (DuplicateOutputs _ op) = [op]
getChildOps (MapOp _ op) = [op]
getChildOps (ReduceOp _ _ op) = [op]
getChildOps (NoOp _) = []
getChildOps (LogicalUtil _ op) = [op]
getChildOps (Register _ _ _) = []
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

