module STAST where
import STTypes
import STMetrics

-- These are leaf nodes that can be used in a higher order operator
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
  -- Ints are the entries in the table. First entry is index 0
  | LUT [Int]
  | MemRead TokenType
  | MemWrite TokenType
  -- first arg is pixels per clock in each dimension. First value in list is outer 
  -- most dimension that iterating over (rows first, columns second in 2d case) 
  -- second arg is window width in each dimension. Same indexing order 
  -- third arg is the size of the image. Saem indexing order. This is necessary
  -- for internal buffer sizing
  -- Last is the type of the pixel element
  | LineBuffer {pxPerClock :: [Int], windowWidth :: [Int], image :: [Int], lbInT :: TokenType}
  -- Array is constant produced, int is sequence length
  | Constant_Int {intConstProduced :: [Int]}
  -- Array is constant produced
  | Constant_Bit {bitConstProduced :: [Bool]}

  -- TYPE MANIPULATORS
  --
  -- Reshapes an input array sequence through space and time.  Buffers
  -- inputs (left-to-right) and emits output only when sufficient
  -- outputs are ready.  Args: input tuple, output tuple, array entry
  -- type. Tuple consists of (sequence length, array length) for the
  -- one input or one output port. Array type may be another array; if
  -- so, it's treated as atomic and not split between two output
  -- cycles.
  | SequenceArrayRepack (Int, Int) (Int, Int) TokenType
  -- First is list of input port types, second is output.
  -- Pure combinational device: decomposes the input and output arrays to
  -- a sequence of wires, and wires up inputs to outputs in order.
  | ArrayReshape [TokenType] [TokenType]
  | DuplicateOutputs Int Op


  -- HIGHER ORDER OPS
  | MapOp {mapParallelism :: Int, mappedOp :: Op}
  | ReduceOp {reduceParallelism :: Int, reduceNumCombined :: Int, reducedOp :: Op}

  -- TIMING HELPERS
  | NoOp [TokenType]
  -- this removes some of the outputs of the wrapped module
  -- list of n to drop is doubly nested:
  -- outer list is for each of the output ports of the wrapped module
  -- inner list for dropping and keeping tokens. First part of tuple is
  -- n to drop, second is n to keep. Creating patterns with lists of tuples
  -- this unit only impacts hardware for stateful elements by making valid
  -- false. It is only for aetherling type manipulations in combinational
  -- units
  -- ASSUMING for each port, droopedInDKPairs + keptInDKPairs for its list
  -- is a multiple of its sequence length 
  | Crop {crops :: [[DropKeepPair]], scaledCPS :: Int, croppedOp :: Op}
  -- crop and delay aren't perfectly symmetric. Crop changes the number of
  -- elements for each output port independently, while delay affects the
  -- clock and thus impacts all ports by same number of clocks
  -- this unit only impacts hardware for stateful elements by delaying clock
  -- enable. It is only for aetherling type manipulations in combinational
  -- units
  -- Assuming droppedInDKPairs + keptInDKPairs is a multiple of delayedOP's cps
  | Delay {delays :: [DropKeepPair], delayedOp :: Op} 
  -- run underOp at CPS = utilDenominator * old CPS
  -- this is essentially a multiplier version of delay. It is separate as
  -- this is used more to slowdown and delay is used to match warmups
  | Underutil {utilDenominator :: Int, underutilizedOp :: Op}
  -- this increases latency
  | RegRetime {retimeClocks :: Int, retimeddOp :: Op}

  -- COMPOSE OPS
  | ComposePar [Op]
  | ComposeSeq [Op]
  | Failure FailureType 
  deriving (Eq, Show)

-- for a sequence of clocks or tokens,
-- a list of pairs where each first element is a number dropped
-- and the second is a number of clocks or tokens kept
data DropKeepPair = DKPair {numDropped :: Int, numKept :: Int}
  deriving (Eq, Show)

-- given a list of DKPairs, get the total number of dropped clocks
droppedInDKPairs :: [DropKeepPair] -> Int
droppedInDKPairs dkPairs = foldl (+) 0 $ map numDropped dkPairs

keptInDKPairs :: [DropKeepPair] -> Int
keptInDKPairs dkPairs = foldl (+) 0 $ map numKept dkPairs

-- given an Op and a list of DKPairs, modify all its ports according to
-- modifySeqLenForDropKeepPairs
getPortsWithDKPairsApplied :: [PortType] -> [DropKeepPair] -> [PortType]
getPortsWithDKPairsApplied ports dkPairs =
  map (\port -> modifySeqLenForDropKeepPairs port dkPairs) ports

-- given a Crop's DKPairs for a port, scale the port's seqlen up to
-- the sum of the dropped and kept length and drop the dropped tokens
modifySeqLenForDropKeepPairs :: PortType -> [DropKeepPair] -> PortType
modifySeqLenForDropKeepPairs (T_Port name _ t pct) dkPairs =
  T_Port name (keptInDKPairs dkPairs) t pct

data FailureType =
  ComposeFailure ComposeResult (Op, Op)
  | InvalidThroughputModification {attemptedMult :: Int, actualMult :: Int}
  deriving (Eq, Show)

-- SeqPortMismatch indicates couldn't do comopse as composeSeq requires 
-- all port types and latencies 
data ComposeResult = 
  PriorFailure 
  | SeqPortMismatch {outPortsThroughput :: [PortThroughput], inPortsThroughput :: [PortThroughput]}
  | ComposeSuccess
  deriving (Eq, Show)

-- debugging help methods for parsing syntax tree
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
getChildOps (LineBuffer _ _ _ _) = []
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayRepack _ _ _) = []
getChildOps (ArrayReshape _ _) = []
getChildOps (DuplicateOutputs _ op) = [op]
getChildOps (MapOp _ op) = [op]
getChildOps (ReduceOp _ _ op) = [op]
getChildOps (NoOp _) = []
getChildOps (Crop _ _ op) = [op]
getChildOps (Delay _ op) = [op]
getChildOps (Underutil _ op) = [op]
getChildOps (RegRetime _ op) = [op]
getChildOps (ComposePar ops) = ops
getChildOps (ComposeSeq ops) = ops
getChildOps (Failure (ComposeFailure _ (op0, op1))) = [op0, op1]
getChildOps (Failure _) = []

-- Walk the failure tree and find the first one, preferring failures on the left
-- over the right
-- Will return the parent node if not failures
isFailure (Failure _) = True
isFailure _ = False
hasChildWithError op = (<) 0 $ length $
  filter (\i -> isFailure i || hasChildWithError i) $ getChildOps op
getFirstError op | hasChildWithError op = head $ map getFirstError $ getChildOps op
getFirstError op | isFailure op = op
getFirstError op = op
