module Aetherling.Simulator.Arrays (
    simhlRepack,
    simhlReshape,
    simhlPreRepack,
    simhlPreReshape
) where
import Data.Array
import Data.List
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Simulator.Combinational
import Aetherling.Simulator.State

-- Simulator and preprocessor pass implementations for
-- SequenceArrayRepack, ArrayReshape, and LineBuffer.

-- | Simulator implementation function for ArrayReshape (reshapes
-- sequence of arrays through space and time).
simhlRepack :: (Int,Int) -> (Int,Int) -> TokenType -> [[ValueType]]
            -> [[ValueType]]
simhlRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t [inStr] =
    if inSeqLen * inWidth /= outSeqLen * outWidth || inSeqLen * inWidth == 0
    then error("Aetherling internal error: repack I/O throughput mismatch.")
    else
      let allInputs = simhlRepackUnpack inWidth inStr
      in [simhlRepackRepack outWidth allInputs]
simhlRepack _ _ _ _ = error "Aetherling internal error: broken array repack."

-- Glue together all inputs, ordered by time then by left-to-right.
simhlRepackUnpack :: Int -> [ValueType] -> [ValueType]
simhlRepackUnpack inWidth [] = []
simhlRepackUnpack inWidth (V_Unit:futureArrays) =
    (replicate inWidth V_Unit)++(simhlRepackUnpack inWidth futureArrays)
simhlRepackUnpack inWidth ((V_Array array):futureArrays) =
    if length array == inWidth
    then array ++ (simhlRepackUnpack inWidth futureArrays)
    else error "Aetherling internal error: wrong array length in repack."
simhlRepackUnpack _ _ = error "Aetherling internal error: broken array unpack."

-- Split up all input through time and space into a stream of output arrays
-- of length outSeqLen. Truncate leftover output.
simhlRepackRepack :: Int -> [ValueType] -> [ValueType]
simhlRepackRepack outWidth values =
    let (nowArray, futureValues) = splitAt outWidth values
    in
      if length nowArray == outWidth
      then (V_Array nowArray):(simhlRepackRepack outWidth futureValues)
      else []

-- | Combinational device that decomposes arrays into fundamental types
-- and puts them back together in a different order. This function
-- takes an ArrayReshape Op and returns an implementation function
-- suitable for simhlCombinational (list of in port values in one
-- clock -> out port values in one clock).
simhlReshape :: Op -> [ValueType] -> [ValueType]
simhlReshape (ArrayReshape inTypes outTypes) nowInputs =
    let serial = concat $ map (uncurry simhlSerializeArray)
                                   (zip inTypes nowInputs)
    in simhlDeserializeArrays (ArrayReshape inTypes outTypes) serial
simhlReshape _ _ =
    error "Aetherling internal error: expected ArrayReshape Op."

-- Take one instance of (possible nested) V_Arrays and a TokenType
-- describing the intended type of the array, and recursively flatten
-- it down to a list of ValueType. If we get a V_Unit in place
-- of some array input, we need to make sure we generate N1*N2*...
-- copies of the V_Unit, where N1, N2... are the dimensions of the array.
-- Note: The line buffer depends on this function too.
simhlSerializeArray :: TokenType -> ValueType -> [ValueType]
simhlSerializeArray (T_Array 0 _) _ = []
simhlSerializeArray (T_Array n t) V_Unit =
    (simhlSerializeArray t V_Unit)
     ++ simhlSerializeArray (T_Array (n-1) t) V_Unit
simhlSerializeArray (T_Array n t) (V_Array (aHead:aTail)) =
    (simhlSerializeArray t aHead)
     ++ (simhlSerializeArray (T_Array (n-1) t) (V_Array aTail))
simhlSerializeArray t value = [value]


-- Takes the ArrayReshape op and a list of serialized values, and
-- packs it into a list of V_Arrays (or scalar types) depending on
-- the output types of the ArrayReshape.
simhlDeserializeArrays :: Op -> [ValueType]
                       -> [ValueType]
simhlDeserializeArrays (ArrayReshape inTypes outTypes) serialValues =
    let
      initTuple = (ArrayReshape inTypes outTypes, [], serialValues)
      (_, result, _) = foldl' simhlDeserializeLambda initTuple outTypes
    in
      result

simhlDeserializeArrays _ _ =
    error "Aetherling internal error: expected ArrayReshape Op."

-- Fold lambda for deserialize.
-- Tuple Op arg is the ArrayReshape being done, used just for error messages.
-- First [ValueType] is the list of V_Arrays (or scalar type) being
-- constructed based on outTypes. Second [ValueType] is the list of serialized
-- values from before. The TokenType tells us what kind of value to construct
-- and append to the first [ValueType] list this time.
simhlDeserializeLambda :: (Op, [ValueType], [ValueType]) -> TokenType
                           -> (Op, [ValueType], [ValueType])
simhlDeserializeLambda (op, packedValues, serialValues) t =
    let (packedValue, leftover) = simhlMunchArray op t serialValues
    in (op, packedValues ++ [packedValue], leftover)

-- Take some of the start of the [ValueType] input and construct a V_Array
-- (or scalar type) based on TokenType. Return the constructed value and
-- unused input as a tuple.
simhlMunchArray :: Op -> TokenType -> [ValueType] -> (ValueType, [ValueType])
simhlMunchArray op (T_Array 0 t) values = (V_Array [], values)
simhlMunchArray op (T_Array n t) values =
    let
      (oneEntry, oneLeftover) = simhlMunchArray op t values
      (V_Array otherEntries, otherLeftover) =
               simhlMunchArray op (T_Array (n-1) t) oneLeftover
    in
      (V_Array (oneEntry:otherEntries), otherLeftover)

simhlMunchArray op t (value:values) =
    if vtTypesMatch value t
    then (value, values)
    else error(
      show value
      ++ " didn't match expected token type "
      ++ show t
      ++ "; either a bug, or something's wrong about the operator "
      ++ show op
      -- This error message is a disgrace. "something's wrong" could be
      -- because there's an int input wired to a bit output or some other
      -- type mismatch, which ArrayReshape doesn't check for.
    )
simhlMunchArray op _ _ =
    error ("Aetherling internal error: broken munch for " ++ show op)

-- | Preprocessor pass implementation for SequenceArrayRepack.
simhlPreRepack :: [Op] -> [Maybe Int] -> SimhlPreState
               -> ([Maybe Int], SimhlPreState)
simhlPreRepack
      opStack@(SequenceArrayRepack (inSeqLen, inWidth) (outSeqLen, outWidth) _ t:_)
      inStrLens
      inState =
    if inSeqLen*inWidth /= outSeqLen*outWidth || inSeqLen*inWidth == 0 then
      error("Need product of sequence length and array width to be nonzero \
            \and equal in input and output at " ++
            (simhlFormatOpStack opStack)
      )
    else
      let
        inStrLen = head inStrLens
        outStrLen' Nothing = Nothing
        outStrLen' (Just n) = Just ((div n inSeqLen) * outSeqLen)
        outStrLen = outStrLen' inStrLen
        warning' Nothing = Nothing
        warning' (Just n) =
          if n `mod` inSeqLen == 0 then Nothing
          else Just "Truncated input (input stream length not divisible by \
                    \input sequence length."
        warning = warning' inStrLen
      in
        simhlPreResult opStack [outStrLen] warning inState
simhlPreRepack _ _ _ =
    error "Aetherling internal error: expected SequenceArrayRepack"

-- | Preprocessor pass implementation for ArrayReshape.
simhlPreReshape :: [Op] -> [Maybe Int] -> SimhlPreState
                -> ([Maybe Int], SimhlPreState)
simhlPreReshape opStack@(ArrayReshape inTypes outTypes:_) inStrLens inState
    | flattenTypes inTypes == flattenTypes outTypes =
      simhlPreCombinational opStack inStrLens inState
    | otherwise =
      error("I/O type mismatch (Bit vs. Int?) at " ++ simhlFormatOpStack opStack)
    where
      flattenTypes :: [TokenType] -> [TokenType]
      flattenTypes (T_Array n t:ts) =
        flattenTypes (replicate n t) ++ flattenTypes ts
      flattenTypes (t:ts) = t:flattenTypes ts
      flattenTypes [] = []
simhlPreReshape _ _ _ =
    error "Aetherling internal error: expected ArrayReshape"

