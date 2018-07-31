{-|
Module: Aetherling.Operations.AST
Description: Aetherling's AST

Provides the Aetherling Abstract Syntax Tree (AST) and functions
for identifying errors in that tree.
-}
module Aetherling.Operations.AST where
import Aetherling.Operations.Types

-- | The operations that can be used to create dataflow DAGs in Aetherling
data Op =
  -- LEAF OPS
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  | Max TokenType
  | Min TokenType
  | Ashr Int TokenType
  | Shl Int TokenType
  | Abs TokenType
  | Not TokenType
  | And TokenType
  | Or TokenType
  | XOr TokenType
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
  | ReduceOp {reduceParallelism :: Int, reduceNumCombined :: Int, reducedOp :: Op}

  -- TIMING HELPERS
  | NoOp [TokenType]
  -- | run underOp at CPS = utilDenominator * old CPS
  | Underutil {utilDenominator :: Int, underutilizedOp :: Op}
  | Delay {delayClocks :: Int, delayedOp :: Op}

  -- COMPOSE OPS
  | ComposePar [Op]
  | ComposeSeq [Op]
  | Failure FailureType 
  deriving (Eq, Show)

data BoundaryConditions = Crop | KeepGarbage deriving (Eq, Show)

data FailureType =
  ComposeFailure ComposeResult (Op, Op)
  | InvalidThroughputModification {attemptedMult :: Int, actualMult :: Int}
  deriving (Eq, Show)

data ComposeResult = 
  PriorFailure 
  -- | SeqPortMismatch indicates couldn't do comopse as composeSeq requires 
  -- all port types and latencies 
  | SeqPortMismatch {outPortsThroughput :: [PortThroughput],
                     inPortsThroughput :: [PortThroughput]}
  | ComposeSuccess
  deriving (Eq, Show)

-- | debugging helper methods for parsing syntax tree
-- get the ops contained inside other ops, for going down ComposeFailure trees
getChildOp n op = getChildOps op !! n
getChildOps :: Op -> [Op]
getChildOps (Add _) = []
getChildOps (Sub _) = []
getChildOps (Mul _) = []
getChildOps (Div _) = []
getChildOps (Max _) = []
getChildOps (Min _) = []
getChildOps (Ashr _ _) = []
getChildOps (Shl  _ _) = []
getChildOps (Abs _) = []
getChildOps (Not _) = []
getChildOps (And _) = []
getChildOps (Or _) = []
getChildOps (XOr _) = []
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
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayRepack _ _ _) = []
getChildOps (ArrayReshape _ _) = []
getChildOps (DuplicateOutputs _ op) = [op]
getChildOps (MapOp _ op) = [op]
getChildOps (ReduceOp _ _ op) = [op]
getChildOps (NoOp _) = []
getChildOps (Underutil _ op) = [op]
getChildOps (Delay _ op) = [op]
getChildOps (ComposePar ops) = ops
getChildOps (ComposeSeq ops) = ops
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